module Vector2 where

import Test.QuickCheck

square :: Num a => a -> a
square x = x * x

data Vector2 a = Vector2 a a
  deriving (Eq, Show)

type Vector2F = Vector2 Float
type Vector2D = Vector2 Double

zero :: Num a => Vector2 a
zero = Vector2 0 0

instance (Arbitrary a) => Arbitrary (Vector2 a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Vector2 x y 

  shrink (Vector2 x y) = [Vector2 x' y' | x' <- shrink x, y' <- shrink y]
                      ++ [Vector2 x y' | y' <- shrink y]
                      ++ [Vector2 x' y | x' <- shrink x]
                      

instance (Num a) => Num (Vector2 a) where
  (+) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
  (-) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

  signum      = undefined
  fromInteger = undefined
  abs         = undefined
  (*)         = undefined
  negate      = undefined

mult :: Num a => a -> Vector2 a -> Vector2 a
mult a (Vector2 x y) = Vector2 (a * x) (a * y)

divide :: Fractional a => a -> Vector2 a -> Vector2 a
divide a (Vector2 x y) = Vector2 (x / a) (y / a)

dot :: Num a => Vector2 a -> Vector2 a -> a
dot (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * x2) + (y1 * y2)

mag :: Floating a => Vector2 a -> a
mag = sqrt . magSquared

magSquared :: Num a => Vector2 a -> a
magSquared (Vector2 x y) = x * x + y * y

dist :: Floating a => Vector2 a -> Vector2 a -> a
dist u v = sqrt $ distSquared u v

distSquared :: Num a => Vector2 a -> Vector2 a -> a
distSquared (Vector2 x1 y1) (Vector2 x2 y2) = square (x2 - x1) + square (y2 - y1)

invert :: Num a => Vector2 a -> Vector2 a
invert (Vector2 x y) = Vector2 (negate x) (negate y)

normalize :: (Ord a, Floating a) => Vector2 a -> Vector2 a
normalize v =
  let m = magSquared v
    in if m > 0
      then sqrt m `divide` v
      else v
