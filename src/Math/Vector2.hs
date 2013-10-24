module Math.Vector2 where

import Math.Algebra

data Vector2 a = Vector2 a a
  deriving (Eq, Show)

(.+) :: a -> a -> Vector2 a
(.+) = Vector2

vmap :: (a -> b) -> Vector2 a -> Vector2 b
vmap f (Vector2 x y) = Vector2 (f x) (f y)

showVector2 :: Show a => Vector2 a -> String
showVector2 (Vector2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

type Vector2F = Vector2 Float
type Vector2D = Vector2 Double

zero :: Num a => Vector2 a
zero = Vector2 0 0

instance Num a => Num (Vector2 a) where
  Vector2 x1 y1 + Vector2 x2 y2 = Vector2 (x1 + x2) (y1 + y2)
  Vector2 x1 y1 - Vector2 x2 y2 = Vector2 (x1 - x2) (y1 - y2)
  Vector2 x1 y1 * Vector2 x2 y2 = Vector2 (x1 * x2) (y1 * y2)

  -- Definition that plays nicely with functions that uses Num a, e.g. sum
  fromInteger i = Vector2 (fromInteger i) (fromInteger i)

  negate (Vector2 x y) = Vector2 (-x) (-y)

  signum = undefined
  abs    = undefined

(.*) :: Num a => a -> Vector2 a -> Vector2 a
(.*) a (Vector2 x y) = Vector2 (a * x) (a * y)

(./) :: Fractional a => Vector2 a -> a -> Vector2 a
(./) (Vector2 x y) a = Vector2 (x / a) (y / a)

dot :: Num a => Vector2 a -> Vector2 a -> a
dot (Vector2 x1 y1) (Vector2 x2 y2) = (x1 * x2) + (y1 * y2)

mag :: Floating a => Vector2 a -> a
mag = sqrt . magSquared

magSquared :: Num a => Vector2 a -> a
magSquared (Vector2 x y) = x * x + y * y

dist :: Floating a => Vector2 a -> Vector2 a -> a
dist u v = sqrt $ distSquared u v

distSquared :: Num a => Vector2 a -> Vector2 a -> a
distSquared (Vector2 x1 y1) (Vector2 x2 y2) =
  square (x2 - x1) + square (y2 - y1)

invert :: Num a => Vector2 a -> Vector2 a
invert (Vector2 x y) = Vector2 (negate x) (negate y)

orthogonal :: Num a => Vector2 a -> Vector2 a
orthogonal (Vector2 x y) = Vector2 (negate y) x

normalize :: (Ord a, Floating a) => Vector2 a -> Vector2 a
normalize v =
  let m = magSquared v
    in if m > 0
      then v ./ sqrt m
      else v

-- |Project u onto v where v is a normalized
project :: (Ord a, Floating a) => Vector2 a -> Vector2 a -> Vector2 a
project u v = let v' = normalize v in (u `dot` v') .* v'
