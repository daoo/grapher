{-# LANGUAGE BangPatterns #-}
module Grapher.Utility
  ( (.$.)
  , alist
  , amap
  , alength
  , aixfold
  , divZero
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.IArray (Ix, IArray, listArray, bounds)

{-# INLINE (.$.) #-}
(.$.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .$. g = \a b -> f (g a b)

{-# INLINE alist #-}
alist :: (IArray a e) => [e] -> a Int e
alist xs = listArray (0, length xs - 1) xs

{-# INLINE amap #-}
amap :: (IArray a e, IArray a e') => (Int -> e -> e') -> a Int e -> a Int e'
amap f arr = listArray bnds $ go a
  where
    bnds@(a, b) = bounds arr

    go !i | i <= b    = f i (arr `unsafeAt` i) : go (i+1)
          | otherwise = []

{-# INLINE alength #-}
alength :: (Ix i, Num i, IArray a e) => a i e -> i
alength = (\(a, b) -> b - a) . bounds

{-# INLINE aixfold #-}
aixfold :: (IArray a e) => (acc -> Int -> e -> acc) -> acc -> a Int e -> acc
aixfold f ini arr = go ini a
  where
    (a, b) = bounds arr

    go !acc !i | i < b     = go (f acc i (arr `unsafeAt` i)) (i+1)
               | otherwise = acc

{-# INLINE divZero #-}
-- |Special division that returns zero when dividing by zero
divZero :: (Num a, Eq a, Fractional a) => a -> a -> a
divZero _ 0 = 0
divZero a b = a / b
