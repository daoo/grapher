{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Grapher.Vector2F
  ( Vector2F((:+))

  , zero
  , (.*)
  , (/.)
  , dot
  , mag
  , mag2
  , dist
  , dist2
  , invert
  , orthogonal
  , normalize
  , project
  ) where

import Data.Vector.Unboxed.Deriving

square :: Float -> Float
square x = x * x

data Vector2F = !Float :+ !Float
  deriving Show

infixr 7 :+

derivingUnbox "Vector2F"
  [t| Vector2F -> (Float, Float) |]
  [| \(x:+y) -> (x, y) |]
  [| uncurry (:+) |]

zero :: Vector2F
zero = 0 :+ 0

instance Num Vector2F where
  (x1 :+ y1) + (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)
  (x1 :+ y1) - (x2 :+ y2) = (x1 - x2) :+ (y1 - y2)
  (x1 :+ y1) * (x2 :+ y2) = (x1 * x2) :+ (y1 * y2)

  -- Definition that plays nicely with functions that uses Num a, e.g. sum
  fromInteger i = fromInteger i :+ fromInteger i

  negate (x :+ y) = (-x) :+ (-y)

  signum = undefined
  abs    = undefined

(.*) :: Float -> Vector2F -> Vector2F
(.*) a (x :+ y) = (a * x) :+ (a * y)

(/.) :: Vector2F -> Float -> Vector2F
(/.) (x :+ y) a = (x / a) :+ (y / a)

dot :: Vector2F -> Vector2F -> Float
dot (x1 :+ y1) (x2 :+ y2) = (x1 * x2) + (y1 * y2)

mag :: Vector2F -> Float
mag = sqrt . mag2

mag2 :: Vector2F -> Float
mag2 (x :+ y) = x * x + y * y

dist :: Vector2F -> Vector2F -> Float
dist u v = sqrt $ dist2 u v

dist2 :: Vector2F -> Vector2F -> Float
dist2 (x1 :+ y1) (x2 :+ y2) =
  square (x2 - x1) + square (y2 - y1)

invert :: Vector2F -> Vector2F
invert (x :+ y) = negate x :+ negate y

orthogonal :: Vector2F -> Vector2F
orthogonal (x :+ y) = negate y :+ x

normalize :: Vector2F -> Vector2F
normalize v =
  let m = mag2 v
    in if m /= 0
      then v /. sqrt m
      else v

-- |Project u onto v where v is a normalized
project :: Vector2F -> Vector2F -> Vector2F
project u v = let v' = normalize v in (u `dot` v') .* v'
