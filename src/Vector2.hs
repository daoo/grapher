module Vector2 where

import Test.QuickCheck

type Vector2F = Vector2 Float
type Vector2D = Vector2 Double

data Vector2 a = Vector2 a a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Vector2 a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Vector2 x y 

  shrink (Vector2 x y) = [Vector2 x' y' | x' <- shrink x, y' <- shrink y]

instance Num a => Num (Vector2 a) where
  (+) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
  (-) (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

  signum      = undefined
  fromInteger = undefined
  abs         = undefined
  (*)         = undefined
  negate      = undefined

dot :: Num a => Vector2 a -> Vector2 a -> a
dot (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * x2) + (y1 * y2)

mag :: Floating a => Vector2 a -> a
mag = sqrt . magSquared

magSquared :: Num a => Vector2 a -> a
magSquared (Vector2 x y) = x * x + y * y
