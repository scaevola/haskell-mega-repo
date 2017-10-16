{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Futurice.App.FUM.Pages.Summary (summaryPage) where

import Futurice.Prelude
import Futurice.IdMap   (IdMap)
import Prelude ()

import Futurice.App.FUM.Markup
import Futurice.App.FUM.Types

import qualified Personio
import qualified Data.Set as Set
import qualified Text.PrettyPrint.Compact as PP

summaryPage
    :: AuthUser
    -> World     -- ^ the world
    -> IdMap Personio.Employee
    -> HtmlPage "summary"
summaryPage auth world personio = fumPage_ "Summary" auth $ do
    -- Title
    fumHeader_ "Summary" []

    row_ $ large_ 12 [ class_ "callout secondary" ]
        "This page summarises data pushed to LDAP"

    fullRow_ $ pre_ $ renderHDoc $
        ppBlock "Employees" (map pp employeesX)
        PP.$$
        ppBlock "Groups" (map pp $ sortOn (view groupName) $ world ^.. worldGroups . folded)

  where
    employeesX :: [EmployeeX]
    employeesX = map mk $ sortOn (view employeeLogin) $ world ^.. worldEmployees . folded
      where
        mk e = EX e $
            let groups  = world ^.. worldEmployeeGroups . ix (e ^. employeeLogin) . folded . groupName
                sgroups = mcase (personio ^? ix (e ^. employeePersonioId)) mempty $ \p ->
                    world ^.. worldSpecialGroups
                        . ix (p ^. Personio.employeeEmploymentType, p ^. Personio.employeeOffice, p ^. Personio.employeeTribe)
                        . folded . groupName
            in Set.fromList (groups <> sgroups)

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

newtype H = H (forall m. Monad m => HtmlT m () -> HtmlT m ())

instance Semigroup H where
    H f <> H g = H (f . g)

instance Monoid H where
    mempty = H id
    mappend = (<>)

type HDoc = PP.Doc H

ppBlock :: String -> [HDoc] -> HDoc
ppBlock name = ppBlock' (PP.annotate (H b_) (PP.string name))

ppBlock' :: HDoc -> [HDoc] -> HDoc
ppBlock' nameDoc = PP.hang 4 nameDoc . PP.semiBraces

renderHDoc :: Monad m => HDoc -> HtmlT m ()
renderHDoc = PP.renderWith PP.Options
    { PP.optsPageWidth = 120
    , PP.optsAnnotate = \(H f) str -> f (toHtml str)
    }

infix 0 <~
(<~) :: Pretty a => String -> a -> HDoc
name <~ x = PP.annotate (H i_) (PP.string name) PP.<+> PP.char '=' PP.<+> pp x

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

data EmployeeX = EX Employee (Set GroupName)

-------------------------------------------------------------------------------
-- Pretty
-------------------------------------------------------------------------------

class Pretty a where
    pp :: a -> HDoc

instance ann ~ H => Pretty (PP.Doc ann) where
    pp = id

instance Pretty Text where
    pp = PP.string . view unpacked

instance Pretty a => Pretty [a] where
    pp = PP.list . map pp

instance Pretty a => Pretty (Set a) where
    pp = pp . toList

instance Pretty Personio.EmployeeId where
    pp i = PP.annotate (H (const (toHtml i))) (PP.string "xxxxxx") -- TODO!

instance Pretty Login where
    pp l = PP.annotate (H (const (toHtml l))) (pp $ loginToText l)

instance Pretty Email where
    pp x = PP.annotate (H (const (toHtml x))) (pp $ emailToText x)

instance Pretty (UnixID t) where
    pp = PP.string . show . getUnixID

instance Pretty GroupName where
    pp n = PP.annotate (H (const html)) (pp $ groupNameToText n)
      where
        html :: Monad m => HtmlT m ()
        html = a_ [ href_ "#" ] $ toHtml $ groupNameToText n

instance Pretty GroupType where
    pp = pp . groupTypeToText

instance Pretty Status where
    pp = pp . statusToText

instance Pretty EmployeeX where
    pp (EX e gs) = ppBlock' (pp $ e ^. employeeLogin)
        [ "name"     <~ e ^. employeeName
        , "uid"      <~ e ^. employeeUID
        , "personio" <~ e ^. employeePersonioId
        , "status"   <~ e ^. employeeStatus
        , "email"    <~ e ^. employeeEmail
        , "aliases"  <~ e ^. employeeEmailAliases
        , "groups"   <~ gs
        ]

-- TODO: direct members
instance Pretty Group where
    pp e = ppBlock' (pp $ e ^. groupName)
        [ "type"     <~ e ^. groupType
        , "gid"      <~ e ^. groupGID
        , "emails"   <~ e ^. groupEmailAliases
        , "members"  <~ (e ^.. groupEmployees . folded) ++ (e ^.. groupCustomers . folded)
        ]
