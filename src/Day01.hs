module Day01
    ( expenseReport
    , expenseReport'
    ) where

import Data.Foldable (fold, find)

lift3 :: Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f x y z = f <$> x <*> y <*> z

listNumbers :: [Int] -> [(Int, Int)]
listNumbers = fmap sumprod . fold . subsequents where
  subsequents [] = []
  subsequents (x:xs) = (fmap ((,) x) xs) : subsequents xs
  sumprod (x,y) = (x+y, x*y)

listNumbers' :: [Int] -> [(Int, Int)]
listNumbers' xs = sumprod <$> lift3 (,,) xs xs xs  where
  sumprod (x,y,z) = (x+y+z, x*y*z)

findEntry :: [(Int, Int)] -> Maybe Int
findEntry = fmap snd . find ((==2020) . fst)

expenseReport :: IO ()
expenseReport = do
  contents <- readFile "input/01"
  let n = findEntry . listNumbers . fmap read . lines $ contents
  maybe (putStrLn "Not found") print n

expenseReport' :: IO ()
expenseReport' = do
  contents <- readFile "input/01"
  let n = findEntry . listNumbers' . fmap read . lines $ contents
  maybe (putStrLn "not found") print n
