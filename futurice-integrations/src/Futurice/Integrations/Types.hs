{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.Integrations.Types where

import Futurice.Prelude
import Prelude ()

import Futurice.Generics
import Futurice.Report.Columns (ToColumns)
import Futurice.Tribe          (Tribe)

-- | Employee information often used in reports
--
-- /TODO/ lensify
data Employee = Employee
    { employeeName     :: !Text
    , employeeTribe    :: !Tribe
    , employeeContract :: !Text
    }
 deriving (Eq, Show, Typeable, Generic)

instance NFData Employee

deriveGeneric ''Employee

instance ToColumns Employee

instance ToSchema Employee where declareNamedSchema = sopDeclareNamedSchema
instance FromJSON Employee where parseJSON = sopParseJSON
instance ToJSON Employee where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding
