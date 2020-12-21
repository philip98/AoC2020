module Day03
    ( countTrees
    , countTrees'
    ) where

data Grid = Grid Int Int [[Char]]

gridFromString :: String -> Grid
gridFromString [] = Grid 0 0 []
gridFromString s = Grid w h st where
  st = lines s
  h = length st
  w = length . head $ st

height :: Grid -> Int
height (Grid _ h _) = h

lookupPosition :: Int -> Int -> Grid -> Char
lookupPosition x y (Grid w h entries) = entries !! y !! (x `mod` w)

toboggan :: Int -> Int -> Grid -> [Char]
toboggan dx dy grid = go 0 0 where
  go x y | y >= (height grid) = []
         | otherwise = lookupPosition x y grid : go (x+dx) (y+dy)

countTr :: Int -> Int -> Grid -> Int
countTr dx dy = sum . fmap (fromEnum . (=='#')) . toboggan dx dy

countTrees :: IO ()
countTrees = do
  contents <- readFile "input/03"
  let grid = gridFromString contents
  let n = countTr 3 1 grid
  putStrLn $ show n ++ " trees encountered"

countTrees' :: IO ()
countTrees' = do
  contents <- readFile "input/03"
  let grid = gridFromString contents
  let n = product . fmap (flip (uncurry countTr) grid) $ [(1,1), (3,1), (5,1), (7,1), (1,2)]
  putStrLn $ show n ++ " is the product of the numbers of trees encountered"
