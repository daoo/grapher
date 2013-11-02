module ForceGraph.Vector2F where

import ForceGraph.Utility

data Vector2F = Vector2F {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  deriving (Eq, Show)

(.+) :: Float -> Float -> Vector2F
(.+) = Vector2F

vmap :: (Float -> Float) -> Vector2F -> Vector2F
vmap f (Vector2F x y) = Vector2F (f x) (f y)

showVector2F :: Vector2F -> String
showVector2F (Vector2F x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

zero :: Vector2F
zero = Vector2F 0 0

instance Num Vector2F where
  Vector2F x1 y1 + Vector2F x2 y2 = Vector2F (x1 + x2) (y1 + y2)
  Vector2F x1 y1 - Vector2F x2 y2 = Vector2F (x1 - x2) (y1 - y2)
  Vector2F x1 y1 * Vector2F x2 y2 = Vector2F (x1 * x2) (y1 * y2)

  -- Definition that plays nicely with functions that uses Num a, e.g. sum
  fromInteger i = Vector2F (fromInteger i) (fromInteger i)

  negate (Vector2F x y) = Vector2F (-x) (-y)

  signum = undefined
  abs    = undefined

(.*) :: Float -> Vector2F -> Vector2F
(.*) a (Vector2F x y) = Vector2F (a * x) (a * y)

(./) :: Vector2F -> Float -> Vector2F
(./) (Vector2F x y) a = Vector2F (x / a) (y / a)

dot :: Vector2F -> Vector2F -> Float
dot (Vector2F x1 y1) (Vector2F x2 y2) = (x1 * x2) + (y1 * y2)

mag :: Vector2F -> Float
mag = sqrt . mag2

mag2 :: Vector2F -> Float
mag2 (Vector2F x y) = x * x + y * y

dist :: Vector2F -> Vector2F -> Float
dist u v = sqrt $ dist2 u v

dist2 :: Vector2F -> Vector2F -> Float
dist2 (Vector2F x1 y1) (Vector2F x2 y2) =
  square (x2 - x1) + square (y2 - y1)

invert :: Vector2F -> Vector2F
invert (Vector2F x y) = Vector2F (negate x) (negate y)

orthogonal :: Vector2F -> Vector2F
orthogonal (Vector2F x y) = Vector2F (negate y) x

normalize :: Vector2F -> Vector2F
normalize v =
  let m = mag2 v
    in if m /= 0
      then v ./ sqrt m
      else v

-- |Project u onto v where v is a normalized
project :: Vector2F -> Vector2F -> Vector2F
project u v = let v' = normalize v in (u `dot` v') .* v'
