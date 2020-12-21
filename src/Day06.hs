module Day06
    ( sumAnswers
    ) where

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Bits
import Data.Char (isLower, isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as I

type Group = Int

numAffAnswers :: Group -> Int
numAffAnswers = popCount

personParser :: Parser Int
personParser = foldl (+) 0 <$> many1 answer where
  answer = conv <$> satisfy isLower
  conv n = 2^(fromEnum n - fromEnum 'a')

groupParser :: Parser Group
groupParser = foldl (.|.) 0 <$> personParser `sepBy1` endOfLine

groupParser' :: Parser Group
groupParser' = foldl (.&.) (2^26-1) <$> personParser `sepBy1` endOfLine

sumAnswers :: IO ()
sumAnswers = do
  contents <- I.readFile "input/06"
  case parseOnly (groupParser `sepBy` (endOfLine >> endOfLine)) contents of
    Left s -> putStrLn s
    Right answers -> putStrLn $ show (sum $ numAffAnswers <$> answers) ++ " for anyone"
  case parseOnly (groupParser' `sepBy` (endOfLine >> endOfLine)) contents of
    Left s -> putStrLn s
    Right answers -> putStrLn $ show (sum $ numAffAnswers <$> answers) ++ " for everyone"
