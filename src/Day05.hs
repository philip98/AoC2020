module Day05
    ( analyzeIDs
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as I

fromBinary :: [Int] -> Int
fromBinary = foldl c 0 where
  c x y = 2*x + y

rowNumParser :: Parser Int
rowNumParser = fromBinary <$> count 7 (char 'F' *> pure 0 <|> char 'B' *> pure 1)

colNumParser :: Parser Int
colNumParser = fromBinary <$> count 3 (char 'L' *> pure 0 <|> char 'R' *> pure 1)

seatIDParser :: Parser Int
seatIDParser = do
  row <- rowNumParser
  col <- colNumParser
  pure $ row * 8 + col

getIDs :: IO [Int]
getIDs = do
  contents <- I.readFile "input/05"
  case parseOnly (seatIDParser `sepBy` endOfLine) contents of
    Left s -> putStrLn s >> pure []
    Right ids -> pure ids

analyzeIDs :: IO ()
analyzeIDs = do
  ids <- getIDs
  putStrLn $ show (foldl max 0 ids) ++ " is the highest id"
  let possibleIDs = (+1) <$> filter (\c -> (c+1 `notElem` ids) && (c+2 `elem` ids)) ids
  putStrLn $ show possibleIDs ++ " are the possible seat ids"
