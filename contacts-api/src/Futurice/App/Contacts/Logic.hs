{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Futurice.App.Contacts.Logic (
    contacts,
    ) where

import Data.RFC5051          (compareUnicode)
import Futurice.Email        (emailToText)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict   as S
import qualified Data.Text           as T

-- Data definition
import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH
import qualified Personio

-- Contacts modules
import Futurice.App.Contacts.Types
import Futurice.App.Contacts.Types.Tri (lessSure)

compareUnicodeText :: Text -> Text -> Ordering
compareUnicodeText = compareUnicode `on` T.unpack

-- | Default avatar.
noImage :: Text
noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"

-- | Get contacts data
contacts
    :: ( MonadFlowdock m, MonadGitHub m, MonadFUM m, Personio.MonadPersonio m
       , MonadTime m, MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env, HasFlowdockOrgName env
       )
    => m [Contact Text]
contacts = contacts'
    <$> currentTime
    <*> Personio.personio Personio.PersonioEmployees
    <*> fumEmployeeList
    <*> githubDetailedMembers
    <*> flowdockOrganisation

-- | The pure, data mangling part of 'contacts'
contacts'
    :: UTCTime
    -> [Personio.Employee]
    -> Vector FUM.User
    -> Vector GH.User
    -> FD.Organisation
    -> [Contact Text]
contacts' now employees users githubMembers flowdockOrg =
    let employees' = filter (Personio.employeeIsActive now) employees
        res0 = map employeeToContact employees'
        res1 = addGithubInfo githubMembers res0
        res2 = addFlowdockInfo (flowdockOrg ^. FD.orgUsers) res1
        res3 = addFUMInfo users res2
    in sortBy (compareUnicodeText `on` contactName) res3

employeeToContact :: Personio.Employee -> Contact Text
employeeToContact e = Contact
    { contactLogin      = fromMaybe $(FUM.mkLogin "xxxx") $ e ^. Personio.employeeLogin
    , contactFirst      = e ^. Personio.employeeFirst
    , contactName       = e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
    , contactEmail      = maybe "" emailToText $ e ^. Personio.employeeEmail
    , contactPhones     = catMaybes [e ^. Personio.employeeWorkPhone, e ^. Personio.employeeHomePhone]
    , contactTitle      = e ^. Personio.employeePosition
    , contactThumb      = noImage -- from FUM
    , contactImage      = noImage -- from FUM
    , contactFlowdock   = mcase (e ^. Personio.employeeFlowdock) Unknown $
        Sure . (\uid -> ContactFD (fromIntegral $ FD.getIdentifier uid) "-" noImage)
    , contactGithub     = mcase (e ^. Personio.employeeGithub) Unknown $
        Sure . flip (ContactGH . GH.untagName) noImage
    , contactTeam       = e ^. Personio.employeeTribe
    , contactOffice     = e ^. Personio.employeeOffice
    , contactCompetence = e ^. Personio.employeeRole
    , contactExternal   = Just Personio.External == e ^. Personio.employeeEmploymentType
    }

githubDetailedMembers
    :: ( MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.User)
githubDetailedMembers = pure mempty {- do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers
    -}

addFUMInfo
    :: (Functor f, Foldable g)
    => g FUM.User -> f (Contact Text) -> f (Contact Text)
addFUMInfo fum = fmap add
  where
    fum' = toList fum

    loginMap :: HM.HashMap FUM.Login FUM.User
    loginMap = HM.fromList (map pair fum')
      where
        pair x = (FUM._userName x, x)

    add :: Contact Text -> Contact Text
    add c = case HM.lookup (contactLogin c) loginMap of
        Nothing -> c
        Just u  -> c
            { contactThumb = S.fromMaybe noImage $ FUM._userThumbUrl u
            , contactImage = S.fromMaybe noImage $ FUM._userImageUrl u
            }

addGithubInfo
    :: (Functor f, Foldable g)
    => g GH.User -> f (Contact Text) -> f (Contact Text)
addGithubInfo gh = fmap add
  where
    gh' = toList gh

    nameMap :: HM.HashMap Text GH.User
    nameMap = HM.fromList (mapMaybe pair gh')
      where
        pair :: GH.User -> Maybe (Text, GH.User)
        pair x = (\y -> (y, x)) <$> GH.userName x

    loginMap :: HM.HashMap Text GH.User
    loginMap = HM.fromList (map pair gh')
      where
        pair :: GH.User -> (Text, GH.User)
        pair x = (GH.untagName $ GH.userLogin x, x)

    add :: Contact Text -> Contact Text
    add c = c{ contactGithub = (cgh >>= byLogin . cghNick)
                             <> lessSure cgh
                             <> byName }
      where
        cgh :: Tri (ContactGH Text)
        cgh = contactGithub c

        byName :: Tri (ContactGH Text)
        byName = maybe Unknown
                       (Unsure . fromDetailedOwner)
                       (HM.lookup (contactName c) nameMap)

        byLogin :: Text -> Tri (ContactGH Text)
        byLogin ghLogin = maybe Unknown
                                (Sure . fromDetailedOwner)
                                (HM.lookup ghLogin loginMap)

fromDetailedOwner :: GH.User -> ContactGH Text
fromDetailedOwner gh = ContactGH
    { cghNick   = GH.untagName . GH.userLogin $ gh
    , cghAvatar = GH.getUrl $ GH.userAvatarUrl gh
    }

addFlowdockInfo
    :: forall u f g. (FD.UserLike u, Functor f, Foldable g)
    => g u
    -> f (Contact Text)
    -> f (Contact Text)
addFlowdockInfo us = fmap add
  where
    -- we could use ixsed-typed for these
    emailMap :: HM.HashMap Text u
    emailMap = foldMap (\u -> HM.singleton (u ^. FD.userEmail) u) us

    nameMap :: HM.HashMap Text u
    nameMap = foldMap (\u -> HM.singleton (u ^. FD.userName) u) us

    uidMap :: HM.HashMap FD.UserId u
    uidMap = foldMap (\u -> HM.singleton (u ^. FD.userId) u) us

    add :: Contact Text -> Contact Text
    add c = c
        { contactFlowdock =
            (cfd >>= byId . FD.mkIdentifier . fromIntegral . cfdId)
            <> lessSure cfd
            <> byEmail
            <> byName
        }
      where
        cfd = contactFlowdock c
        name = contactName c
        email = contactEmail c

        byId uid = maybe Unknown (Sure . f) (HM.lookup uid uidMap)
        byEmail = maybe Unknown (Sure . f)   (HM.lookup email emailMap)
        byName  = maybe Unknown (Unsure . f) (HM.lookup name nameMap)

        f :: u -> ContactFD Text
        f u = ContactFD
            { cfdId     = fromIntegral $ FD.getIdentifier $ u ^. FD.userId
            , cfdNick   = truncateNick $ u ^. FD.userNick
            , cfdAvatar = u ^. FD.userAvatar
            }

        truncateNick :: Text -> Text
        truncateNick t
            | T.length t >= 20 = T.take 17 t <> "..."
            | otherwise        = t
