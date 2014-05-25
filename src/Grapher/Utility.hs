{-# LANGUAGE BangPatterns #-}
module Grapher.Utility
  ( (.$.)
  , amap
  , aixfold
  , divZero
  ) where

import Data.Array.Base (unsafeAt, unsafeArray, numElements)
import Data.Array.IArray (IArray, bounds)

{-# INLINE (.$.) #-}
(.$.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .$. g = \a b -> f (g a b)

{-# INLINE amap #-}
amap :: IArray a e => (Int -> e -> e) -> a Int e -> a Int e
amap f arr = case bounds arr of
  bnds -> let n = numElements arr
           in unsafeArray bnds [(i, f i (arr `unsafeAt` i)) | i <- [0..n-1]]

{-# INLINE aixfold #-}
aixfold :: IArray a e => (acc -> Int -> e -> acc) -> acc -> a Int e -> acc
aixfold f ini arr = go ini 0
  where
    n = numElements arr

    go !acc !i
      | i < n     = go (f acc i (arr `unsafeAt` i)) (i+1)
      | otherwise = acc

{-# INLINE divZero #-}
-- |Special division operator that returns zero when dividing by zero.
divZero :: (Num a, Eq a, Fractional a) => a -> a -> a
divZero _ 0 = 0
divZero a b = a / b
