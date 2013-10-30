{-# LANGUAGE BangPatterns #-}
module ForceGraph.Utility
  ( times
  , clamp
  , mapIndex
  , square
  , divZero
  ) where

times :: (a -> a) -> Int -> a -> a
times _  0 x = x
times f !i x = times f (i-1) (f x)

clamp :: Ord a => a -> a -> a -> a
clamp a b x | x < a     = a
            | x > b     = b
            | otherwise = x

mapIndex :: (Int -> a -> b) -> [a] -> [b]
mapIndex f = go 0
  where
    go !_ []     = []
    go !i (x:xs) = f i x : go (i+1) xs

square :: Num a => a -> a
square x = x * x

-- |Special division that returns zero when dividing by zero
divZero :: (Num a, Eq a, Fractional a) => a -> a -> a
divZero _ 0 = 0
divZero a b = a / b
