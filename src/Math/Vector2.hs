module Math.Vector2 where

import Math.Algebra

data Vector2D = Vector2D {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show)

(.+) :: Double -> Double -> Vector2D
(.+) = Vector2D

vmap :: (Double -> Double) -> Vector2D -> Vector2D
vmap f (Vector2D x y) = Vector2D (f x) (f y)

showVector2D :: Vector2D -> String
showVector2D (Vector2D x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

zero :: Vector2D
zero = Vector2D 0 0

instance Num Vector2D where
  Vector2D x1 y1 + Vector2D x2 y2 = Vector2D (x1 + x2) (y1 + y2)
  Vector2D x1 y1 - Vector2D x2 y2 = Vector2D (x1 - x2) (y1 - y2)
  Vector2D x1 y1 * Vector2D x2 y2 = Vector2D (x1 * x2) (y1 * y2)

  -- Definition that plays nicely with functions that uses Num a, e.g. sum
  fromInteger i = Vector2D (fromInteger i) (fromInteger i)

  negate (Vector2D x y) = Vector2D (-x) (-y)

  signum = undefined
  abs    = undefined

(.*) :: Double -> Vector2D -> Vector2D
(.*) a (Vector2D x y) = Vector2D (a * x) (a * y)

(./) :: Vector2D -> Double -> Vector2D
(./) (Vector2D x y) a = Vector2D (x / a) (y / a)

dot :: Vector2D -> Vector2D -> Double
dot (Vector2D x1 y1) (Vector2D x2 y2) = (x1 * x2) + (y1 * y2)

mag :: Vector2D -> Double
mag = sqrt . mag2

mag2 :: Vector2D -> Double
mag2 (Vector2D x y) = x * x + y * y

dist :: Vector2D -> Vector2D -> Double
dist u v = sqrt $ dist2 u v

dist2 :: Vector2D -> Vector2D -> Double
dist2 (Vector2D x1 y1) (Vector2D x2 y2) =
  square (x2 - x1) + square (y2 - y1)

invert :: Vector2D -> Vector2D
invert (Vector2D x y) = Vector2D (negate x) (negate y)

orthogonal :: Vector2D -> Vector2D
orthogonal (Vector2D x y) = Vector2D (negate y) x

normalize :: Vector2D -> Vector2D
normalize v =
  let m = mag2 v
    in if m > 0
      then v ./ sqrt m
      else v

-- |Project u onto v where v is a normalized
project :: Vector2D -> Vector2D -> Vector2D
project u v = let v' = normalize v in (u `dot` v') .* v'
