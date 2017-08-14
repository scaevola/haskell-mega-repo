{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types.PersonalIdValidations (
    isValidFinSSN,
    isValidGbNINO,
    isValidSwePIN,
    isValidDeSV,
    isValidDeID
    ) where

import Data.Maybe                  (isJust)
import Text.Regex.Applicative.Text (match, psym)

import Futurice.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import qualified Data.Text           as T

unconsN :: Maybe [a] -> Int -> (Maybe [a], Maybe [a])
unconsN Nothing _    = (Nothing, Nothing)
unconsN (Just xs) n = if length xs < n
    then (Nothing, Nothing)
    else (Just $ take n xs, Just $ drop n xs)

textRegexp 
    :: Int    -- ^ min and max
    -> String -- ^ valid characters 
    -> Text   -- ^ to validate
    -> Maybe String
textRegexp m vChars = match $ range m m (psym (`elem` vChars))
  where
    range
        :: Alternative f
        => Int  -- ^ min
        -> Int  -- ^ max
        -> f a
        -> f [a]
    range mi ma f = go mi ma
      where
        go start end
            | start > end || end <= 0 = pure []
            | start > 0 = (:) <$> f <*> go (start - 1) (end - 1)
            | otherwise = inRange <$> optional f <*> go 0 (end - 1)

        inRange current next = maybe [] (:next) current

validateRange :: Maybe String -> Int -> Int -> Bool
validateRange date mini maxi = case date of
    Nothing -> False
    Just v  -> between v mini maxi
  where
    between d mi ma = case readMaybe d :: Maybe Int of
        Nothing -> False
        Just v  -> v >= mi && v <= ma
    
charsToDigits :: (Read a, Integral a) => String -> Maybe [a]
charsToDigits cs = flipMaybe $ map (readMaybe . (:[])) cs
  where
    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe mbs = 
        let catted = catMaybes mbs
        in if length catted == length mbs
            then Just catted
            else Nothing

numToDigits :: Integral a => a -> [a]
numToDigits = reverse . L.unfoldr f
  where
    f d 
        | d >= 10   = Just (mod d 10, div d 10)
        | d > 0     = Just (d, 0)
        | otherwise = Nothing

-- | Validate Swedish Personal Identity Number
-- 
-- See <https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)>
isValidSwePIN :: Text -> Bool
isValidSwePIN p = foldr (&&) True [ validateRange (fst yer) 1900 9999
                                  , validateRange (fst mnt) 1 12 -- validate month
                                  , validateRange (fst day) 1 31 -- validate day
                                  , validateChecksum pin
                                  ]
  where
    validChars = '-':['0'..'9']
    pin = textRegexp 13 validChars p -- 12 characters and "-" between date and identifier
    yer = unconsN pin 4
    mnt = unconsN (snd yer) 2
    day = unconsN (snd mnt) 2

    validateChecksum piNum = case piNum of
        Just p' -> luhnValidation . charsToDigits $ drop 2 $ filter (/= '-') p'
        Nothing -> False

    -- | Luhn validation
    --
    -- See <https://en.wikipedia.org/wiki/Luhn_algorithm>
    luhnValidation :: Maybe [Int] -> Bool
    luhnValidation mDigits = case mDigits of
        Nothing -> False
        Just ds -> 
            let checkS = last ds
                summed = sum . doubleOdd . reverse $ init ds
            in mod (summed * 9) 10 == checkS
      where
        doubleOdd ds = foldr f [] $ L.zip [1..] ds
          where
            f :: (Int, Int) -> [Int] -> [Int]
            f (i, d) rest
                | even i = d : rest
                | otherwise = (sum . numToDigits) (2 * d) : rest
        
-- | Validate Finnish Social Security Number
--
-- See <https://en.wikipedia.org/wiki/National_identification_number#Finland>
isValidFinSSN :: Text -> Bool
isValidFinSSN snn = foldr (&&) True [ validateRange (fst day) 1 31 -- Validate day
                                    , validateRange (fst mnt) 1 12 -- Validate month
                                    , validateRange (fst yer) 0 99 -- Validate year
                                    , validateCenturyId (fst cid)
                                    , validateRange (fst pid) 0 899 -- validate personal identifier
                                    , validateChecksumId (fst date, fst pid) (fst sid)
                                    ]
  where
    date = unconsN (textRegexp 11 validChars $ T.toUpper $ T.filter (/= ' ') snn) 6
    day = unconsN (fst date) 2
    mnt = unconsN (snd day) 2
    yer = unconsN (snd mnt) 2
    cid = unconsN (snd date) 1
    pid = unconsN (snd cid) 3
    sid = unconsN (snd pid) 1

    validateCenturyId :: Maybe String -> Bool
    validateCenturyId cId = case cId of
        Nothing -> False
        Just i  -> head i `elem` validCentIds

    toDigit :: Char -> Maybe Integer
    toDigit c = maybeFromIntegral $ L.elemIndex c validANums
      where
        maybeFromIntegral :: (Integral a, Num b) => Maybe a -> Maybe b
        maybeFromIntegral (Just v) = Just $ fromIntegral v
        maybeFromIntegral _        = Nothing

    validateChecksumId :: (Maybe String, Maybe String) -> Maybe String -> Bool
    validateChecksumId (Just dt, Just pId) (Just checkS) = 
        case readMaybe (dt ++ pId) :: Maybe Integer of
            Nothing -> False
            Just d  -> case toDigit (head checkS) of
                Nothing -> False
                Just v  -> v == mod d 31 
    validateChecksumId _ _ = False 

    validANums = concat [ ['0'..'9']
                        , ['A'..'F']
                        , ['H']
                        , ['J'..'N']
                        , ['P']
                        , ['R'..'Y']
                        ]
    validCentIds = ['+', '-', 'A']

    validChars = validANums ++ validCentIds

-- | Validate (GB) National Insurance Number
--
-- See <https://www.gov.uk/hmrc-internal-manuals/national-insurance-manual/nim39110>
isValidGbNINO :: Text -> Bool
isValidGbNINO nino = foldr (&&) True [ validatePrefix (fst pre)
                                     , validateMiddle (fst mid)
                                     , validateSuffix (fst suf)
                                     ]
  where
    validChars = validLetters ++ ['0'..'9']
    nin = textRegexp 9 validChars $ T.toUpper $ T.filter (/= ' ') nino
    pre = unconsN nin 2
    mid = unconsN (snd pre) 6
    suf = unconsN (snd mid) 1

    validatePrefix :: Maybe String -> Bool
    validatePrefix pref = case pref of
        Nothing -> False
        Just p  -> isJust (textRegexp 2 validPreLetters (T.pack p)) 
                   && last p /= 'O'
                   && p `notElem` ["BG", "GB", "KN", "NK", "NT", "TN", "ZZ"]
      where
        validPreLetters = 
            filter (`notElem` ['D', 'F', 'I', 'Q', 'U', 'V']) validLetters

    validateMiddle :: Maybe String -> Bool
    validateMiddle middle = case middle of
        Nothing -> False
        Just m  -> isJust $ textRegexp 6 ['0'..'9'] $ T.pack m

    validateSuffix :: Maybe String -> Bool
    validateSuffix suff = case suff of
        Nothing -> False
        Just s  -> isJust $ textRegexp 1 validLetters $ T.pack s

    validLetters = ['A'..'Z']

-- | Validate (DE) Social security number 
-- 
-- See <https://www.financescout24.de/wissen/ratgeber/sozialversicherungsnummer#aufbau>
isValidDeSV :: Text -> Bool
isValidDeSV sv = foldr (&&) True [ validateRange (fst day) 1 31 
                                 , validateRange (fst mnt) 1 12
                                 , validateRange (fst yer) 0 99
                                 , validInitial (fst ini)
                                 , checksum $ initialToDigit svn (fst ini)
                                 ]
  where
    validLetters = ['A'..'Z']
    validChars = ['0'..'9'] ++ validLetters
    svn = textRegexp 12 validChars $ T.toUpper $ T.filter (/= ' ') sv
    pns = unconsN svn 2 -- responsible pension insurer
    day = unconsN (snd pns) 2
    mnt = unconsN (snd day) 2
    yer = unconsN (snd mnt) 2
    ini = unconsN (snd yer) 1

    validInitial :: Maybe String -> Bool
    validInitial initial = case initial of
        Nothing -> False
        Just i  -> head i `elem` validLetters

    initialToDigit :: Maybe String -> Maybe String -> Maybe [Integer]
    initialToDigit (Just svNum) (Just initial) = case (pre, mid, suf) of
        (Just p, Just m, Just s) -> Just $ p ++ m ++ s
        _                        -> Nothing
      where
        mid = maybe Nothing f (L.elemIndex (head initial) ['A'..'Z'])
          where
            f x = if x + 1 > 9
                then Just $ (numToDigits . fromIntegral) (x + 1)
                else Just [0, fromIntegral (x + 1)]
        pre = charsToDigits (take 8 svNum)
        suf = charsToDigits (drop 9 svNum)
    initialToDigit _ _ = Nothing

    checksum :: Maybe [Integer] -> Bool
    checksum digits = 
        case digits of
            Nothing -> False
            Just ds -> 
                let checkD = last ds
                in mod (summed ds) 10 == checkD
      where
        summed ds = sum $ map (sum . numToDigits) $ L.zipWith (*) (init ds) multipliers
        multipliers = [2, 1, 2, 5, 7, 1, 2, 1, 2, 1, 2, 1]

-- | Validate (DE) ID number
-- 
-- See <https://de.wikipedia.org/wiki/Steuerliche_Identifikationsnummer#Aufbau_der_Identifikationsnummer>
-- and <http://www1.osci.de/sixcms/media.php/13/Pr%FCfziffernberechnung.pdf>
isValidDeID :: Text -> Bool
isValidDeID deId = foldr (&&) True [ validInitial $ fst initial
                                   , oneTwiceOrThrice occurred
                                   , allLeqThrice occurred
                                   , validateChecksum dId 
                                   ]
  where
    dId = maybe Nothing charsToDigits $ textRegexp 11 ['0'..'9'] $ T.filter (/= ' ') deId
    initial = unconsN dId 1
    noCheckS = unconsN dId 10
    occurred = case fst noCheckS of
        Just ds -> HM.toList $ HM.fromListWith (+) [(c, 1) | c <- ds]
        _       -> []

    allLeqThrice :: [(Integer, Integer)] -> Bool
    allLeqThrice ocrd = not (any f ocrd)
      where
        f (_, n) = n > 3

    oneTwiceOrThrice :: [(Integer, Integer)] -> Bool
    oneTwiceOrThrice ocrd = length (filter f ocrd) == 1
      where
        f (_, n) = 3 == n || n == 2

    validInitial :: Maybe [Integer] -> Bool
    validInitial ds = case ds of
        Just [x] -> x /= 0
        _        -> False
        
    validateChecksum :: Maybe [Integer] -> Bool
    validateChecksum digits = case digits of
        Nothing -> False
        Just ds -> checksum (init ds) == last ds

    checksum :: [Integer] -> Integer
    checksum digits = checkCipher
      where
        n = 11
        m = 10
        checkCipher = if n - prdct == 10 then 0 else n - prdct
        prdct = foldl prdctCount m digits
        
        prdctCount
            :: Integer -- cipher
            -> Integer -- old product
            -> Integer -- new product
        prdctCount cipher prd = mod (2 * sm cipher prd) n

        sm cipher prd = let s = mod (cipher + prd) m
                        in if s == 0 then m else s
