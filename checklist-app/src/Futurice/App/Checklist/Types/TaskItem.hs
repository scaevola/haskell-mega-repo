{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskItem where

import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger      (SwaggerType (SwaggerString), enum_, type_)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Map        as Map
import qualified Data.Text       as T
import qualified FUM.Types.Login as FUM

-- | States of the tasks
data TaskItem
    = TaskItemDone
    | TaskItemTodo
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''TaskItem
deriveGeneric ''TaskItem

-- | Annotated task item with who and when have done it.
data AnnTaskItem
    = AnnTaskItemDone !Text !FUM.Login !UTCTime
    | AnnTaskItemTodo !Text
  deriving (Eq, Ord, Show, Typeable, Generic)

annTaskItemTodo :: AnnTaskItem
annTaskItemTodo = AnnTaskItemTodo ""

annTaskItemComment :: Lens' AnnTaskItem Text
annTaskItemComment = lens g s
  where
    g (AnnTaskItemDone t _ _) = t
    g (AnnTaskItemTodo t)     = t
    s (AnnTaskItemDone _ x y) t = AnnTaskItemDone t x y
    s (AnnTaskItemTodo _)     t = AnnTaskItemTodo t

makePrisms ''AnnTaskItem

instance Arbitrary TaskItem where
    arbitrary = sopArbitrary
    shrink    = sopShrink

taskItemToText :: TaskItem -> Text
taskItemToText TaskItemDone = "done"
taskItemToText TaskItemTodo = "todo"

taskItemFromText :: Text -> Maybe TaskItem
taskItemFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ taskItemToText x, x)) [minBound .. maxBound]

_TaskItem :: Prism' Text TaskItem
_TaskItem = prism' taskItemToText taskItemFromText

instance ToParamSchema TaskItem where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (String . taskItemToText <$> [minBound .. maxBound])

instance ToJSON TaskItem where
    toJSON = String . taskItemToText

instance FromJSON TaskItem where
    parseJSON = withText "TaskItem" $ \t ->
        maybe (fail $ "invalid taskItem " <> t ^. unpacked) pure $ t ^? _TaskItem

instance FromHttpApiData TaskItem where
    parseUrlPiece t =
        maybe (Left $ "invalid taskItem " <> t) Right $ t ^? _TaskItem

instance ToHttpApiData TaskItem where
    toUrlPiece = taskItemToText
