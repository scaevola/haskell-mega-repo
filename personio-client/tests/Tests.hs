{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import Data.Aeson.Compat
import Data.Aeson.Types      (parseEither)
import Futurice.Prelude
import Futurice.Tribe        (mkTribe)
import Futurice.Office (Office (..))
import Prelude ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Aeson.Lens (key, _String)

import Personio
import Personio.Types.EmployeeEmploymentType

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ properties
    , examples
    , isValidIBANTests
    ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "QuickCheck"
    [ testProperty "Employee roundtrip" employeeAesonRoundtrip
    ]

employeeAesonRoundtrip :: Employee -> Property
employeeAesonRoundtrip e = lhs === rhs
  where
    lhs = eitherDecode (encode e)
    rhs = Right e

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

correctEmployeeValue :: Value
correctEmployeeValue =
    $(makeRelativeToProject "fixtures/employee.json" >>= embedFromJSON (Proxy :: Proxy Value))
        & key "attributes" 
            . key "department" . key "value" . key "attributes" . key "name" . _String .~ "Tammerforce"
        & key "attributes"
            . key "office"     . key "value" . key "attributes" . key "name" . _String .~ "Tampere"

examples :: TestTree
examples = testGroup "HUnit"
    [ testCase "parsePersonioEmployee" $ do
        e <- either fail pure $
            parseEither parsePersonioEmployee correctEmployeeValue
        "Teemu" @=? e ^. employeeFirst
        "Teekkari" @=? e ^. employeeLast
        Just $(mkDay "2017-05-29") @=? e ^. employeeHireDate
        Nothing @=? e ^. employeeEndDate
        "Developer (Primary)" @=? e ^. employeeRole
        "teemu.teekkari@example.com" @=? e ^. employeeEmail
        "+123 5678910" @=? e ^. employeePhone
        Just (EmployeeId 1337) @=? e ^. employeeSupervisorId
        $(mkTribe "Tammerforce") @=? e ^. employeeTribe
        OffTampere @=? e ^. employeeOffice
        Just "gitMastur" @=? e ^. employeeGithub
        Nothing @=? e ^. employeeFlowdock
        Active @=? e ^. employeeStatus
        Just 0 @=? e ^. employeeHRNumber
        Internal @=? e ^. employeeEmploymentType
    , validations
    ]

-------------------------------------------------------------------------------
-- Validations
-------------------------------------------------------------------------------

-- | 
-- @
-- attributeValue :: Text -> Traversal' Value Value
-- @
attributeValue :: Applicative f => Text -> (Value -> f Value) -> Value -> f Value
attributeValue k = key "attributes" . key k . key "value"

validations :: TestTree
validations = testGroup "Validations"
    [ testValidation
        "GitHub"
        (GithubInvalid "http://github.com/gitMastur")
        $ correctEmployeeValue
            & attributeValue "dynamic_72913" . _String
                .~ "http://github.com/gitMastur"
    , testValidation
        "email"
        EmailMissing
        $ correctEmployeeValue
            & attributeValue "email" .~  Array mempty
    , testValidation
        "tribe"
        TribeMissing
        $ correctEmployeeValue
            & attributeValue "department" .~  Array mempty
    , testValidation
        "cost center"
        CostCenterMissing
        $ correctEmployeeValue -- TODO!
    , testValidation
        "office"
        OfficeMissing
        $ correctEmployeeValue
            & attributeValue "office" .~  Array mempty
    , testValidation
        "phone"
        PhoneMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_27163" . _String .~  ""
    , testValidation
        "role"
        RoleMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72910" . _String .~  ""
    , testValidation
        "IBAN"
        IbanInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72976" . _String .~  "GB82 WEST 1234 568 7654 32"
    , testValidation
        "login name"
        (LoginInvalid "CAPS")
        $ correctEmployeeValue
            & attributeValue "dynamic_72982" . _String .~  "CAPS"
    , testValidation
        "employment_type missing"
        EmploymentTypeMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~  ""
{-
    , testValidation
        "employment_type invalid"
        EmploymentTypeMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~  "wrong"
-}
    , testValidation
        "external contract_end_date"
        ExternalEndDateMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~  "external"
            & attributeValue "contract_end_date" .~ Null
    ]
  where
    testValidation name warning val = testCase name $ do
        ev <- either fail pure $ parseEither validatePersonioEmployee val
        assertBool (show ev) $ warning `elem` ev ^. evMessages

-------------------------------------------------------------------------------
-- IBAN
-------------------------------------------------------------------------------

isValidIBANTests :: TestTree
isValidIBANTests = testGroup "isValidIBAN"
    [ validIBAN "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
    , invalidIBAN "FI21 Ä234 5600 0007 foo" -- invalid chars
    , invalidIBAN "FI21 1234 5600 00" -- too short
    , invalidIBAN "MT84 MALT 0110 0001 2345 MTLC AST0 01S 00" -- too long
    , invalidIBAN "FI21 4321 5600 0007 85" -- wrong checksum
    ]
  where
    validIBAN :: Text -> TestTree
    validIBAN iban = testCase (iban ^. unpacked ++ " is valid") $
        assertBool "invalid!" $ isValidIBAN iban

    invalidIBAN :: Text -> TestTree
    invalidIBAN iban = testCase (iban ^. unpacked ++ " is invalid") $
        assertBool "valid!" $ not $ isValidIBAN iban
