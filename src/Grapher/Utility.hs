module Grapher.Utility
  ( (.$.)
  , divZero
  ) where

{-# INLINE (.$.) #-}
(.$.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .$. g = \a b -> f (g a b)

{-# INLINE divZero #-}
-- |Special division operator that returns zero when dividing by zero.
divZero :: (Num a, Eq a, Fractional a) => a -> a -> a
divZero _ 0 = 0
divZero a b = a / b
