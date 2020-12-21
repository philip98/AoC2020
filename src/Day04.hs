{-# LANGUAGE OverloadedStrings #-}
module Day04
    ( countPassports
    ) where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Char (isHexDigit, isSpace)
import Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Lib (lift2, split, filterMap)

type Passport = M.HashMap T.Text T.Text

isValid :: Passport -> Bool
isValid p = and $ (maybe False (const True) . (flip M.lookup p)) <$> ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

kvSpace :: Parser ()
kvSpace = const () <$> (many (char ' ') `sepBy` char '\n')

passportParser :: Parser Passport
passportParser = M.fromList <$> (kv `sepBy` kvSpace) where
  kv = do
    skipSpace
    key <- takeTill (==':')
    char ':'
    value <- takeTill isSpace
    pure (key, value)

rangeValidator :: Int -> Int -> String -> Parser T.Text
rangeValidator lower upper err = do
  n <- decimal
  if lower <= n && n <= upper
    then pure . T.pack $ show n
    else fail err

passportParser' :: Parser Passport
passportParser' = M.fromList <$> (kv `sepBy` kvSpace) where
  kv = do
    skipSpace
    key <- takeTill (==':')
    char ':'
    value <- case key of
      "byr" -> rangeValidator 1920 2002 "Invalid birth year"
      "iyr" -> rangeValidator 2010 2020 "Invalid issue year"
      "eyr" -> rangeValidator 2020 2030 "Invalid expiration year"
      "hgt" -> lift2 (<>) (rangeValidator 150 193 "Invalid height (cm)") (string "cm")
        <|> lift2 (<>) (rangeValidator 59 76 "Invalid height (in)") (string "in")
      "hcl" -> T.pack <$> (lift2 (:) (char '#') (count 6 (satisfy isHexDigit)))
      "ecl" -> choice ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      "pid" -> T.pack <$> count 9 digit
      "cid" -> takeTill isSpace
      otherwise -> fail "Invalid key"
    pure (key, value)
  byrParser = rangeValidator 1920 2002 ""

parsePassports :: Parser Passport -> T.Text -> [Passport]
parsePassports par =
  filterMap (toMaybe . parseOnly par . T.unlines) . split (==T.empty) . T.lines where
    toMaybe = either (const Nothing) Just

countPassports :: IO ()
countPassports = do
  contents <- I.readFile "input/04"
  let
    n = sum . fmap (fromEnum . isValid) . parsePassports passportParser $ contents
    n' = sum . fmap (fromEnum . isValid) . parsePassports passportParser' $ contents
  putStrLn $ show n ++ " valid passports"
  putStrLn $ show n' ++ " strictly valid passports"
