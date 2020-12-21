module Day02
    ( checkPasswords
    , checkPasswords'
    ) where

import qualified Data.Text.IO as I
import qualified Data.Text as T
import Data.Attoparsec.Text

data PasswordPolicy = PasswordPolicy Int Int Char
  deriving Show

isValid :: PasswordPolicy -> T.Text -> Bool
isValid (PasswordPolicy minCt maxCt c) = withinBounds minCt maxCt . T.count (T.singleton c) where
  withinBounds lower upper x = lower <= x && x <= upper

isValid' :: PasswordPolicy -> T.Text -> Bool
isValid' (PasswordPolicy minCt maxCt c) password
  = (T.index password (minCt-1) == c) /= (T.index password (maxCt-1) == c)

parseLine :: T.Text -> Either String (PasswordPolicy, T.Text)
parseLine = parseOnly $ do
  lower <- decimal
  char '-'
  upper <- decimal
  space
  c <- letter
  char ':'
  space
  pw <- takeText
  pure ((PasswordPolicy lower upper c), pw)


checkPasswords :: IO ()
checkPasswords = do
  contents <- I.readFile "input/02"
  let n = sum . fmap (either (const 0) (fromEnum . uncurry isValid) . parseLine) . T.lines $ contents
  putStrLn $ show n ++ " passwords are valid"

checkPasswords' :: IO ()
checkPasswords' = do
  contents <- I.readFile "input/02"
  let n = sum . fmap (either (const 0) (fromEnum . uncurry isValid') . parseLine) . T.lines $ contents
  putStrLn $ show n ++ " passwords valid according to the new standard"
