module Lib
    ( lift2
    , lift3
    , filterMap
    , split
    ) where

split :: (a -> Bool) -> [a] -> [[a]]
split p l = case break p l of
    (h, []) -> [h, []]
    (h, (t:ts)) -> h : split p ts

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap p [] = []
filterMap p (x:xs) = case p x of
    Just y -> y : filterMap p xs
    Nothing -> filterMap p xs

lift2 :: Applicative m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = f <$> x <*> y

lift3 :: Applicative m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f x y z = f <$> x <*> y <*> z
