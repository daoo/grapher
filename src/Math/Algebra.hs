module Math.Algebra
  ( square
  , divZero
  ) where

square :: Num a => a -> a
square x = x * x

-- |Special division that returns zero when dividing by zero
divZero :: (Num a, Eq a, Fractional a) => a -> a -> a
divZero _ 0 = 0
divZero a b = a / b
