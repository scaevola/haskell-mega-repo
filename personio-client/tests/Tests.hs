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
        Just $(mkTribe "Tammerforce") @=? e ^. employeeTribe
        Just OffTampere @=? e ^. employeeOffice
        Just "gitMastur" @=? e ^. employeeGithub
        Active @=? e ^. employeeStatus
        Just 0 @=? e ^. employeeHRNumber
        Internal @=? e ^. employeeEmploymentType
    , validations
    ]

-------------------------------------------------------------------------------
-- Validations
-------------------------------------------------------------------------------

validations :: TestTree
validations = testGroup "Validations"
    [ testValidationV
        "GitHub"
        (GithubInvalid "http://github.com/gitMastur")
        $ correctEmployeeValue
            & key "attributes" . key "dynamic_72913" . key "value" . _String
                .~ "http://github.com/gitMastur"
    , testValidation
        "email"
        $(makeRelativeToProject "fixtures/employee-m-email.json" >>= embedFile)
        EmailMissing
    , testValidation
        "tribe"
        $(makeRelativeToProject "fixtures/employee-m-tribe.json" >>= embedFile)
        TribeMissing
    , testValidation
        "cost center"
        $(makeRelativeToProject "fixtures/employee.json" >>= embedFile)
        CostCenterMissing
    , testValidation
        "office"
        $(makeRelativeToProject "fixtures/employee-m-office.json" >>= embedFile)
        OfficeMissing
    , testValidation
        "phone"
        $(makeRelativeToProject "fixtures/employee-m-phone.json" >>= embedFile)
        PhoneMissing
    , testValidation
        "role"
        $(makeRelativeToProject "fixtures/employee-m-role.json" >>= embedFile)
        RoleMissing
    , testValidation
        "IBAN"
        $(makeRelativeToProject "fixtures/employee-i-iban.json" >>= embedFile)
        IbanInvalid
    , testValidation
        "login name"
        $(makeRelativeToProject "fixtures/employee-i-login.json" >>= embedFile)
        $ LoginInvalid "erAt"
    , testValidation
        "employment_type"
        $(makeRelativeToProject "fixtures/employee-m-etype.json" >>= embedFile)
        EmploymentTypeMissing
    , testValidation
        "external contract_end_date"
        $(makeRelativeToProject "fixtures/employee-m-endd.json" >>= embedFile)
        ExternalEndDateMissing
    ]
  where
    -- TODO: remove me
    testValidation name source warning = testCase name $ do
        contents <- decodeStrict source
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $ warning `elem` ev ^. evMessages

    testValidationV name warning val = testCase name $ do
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
