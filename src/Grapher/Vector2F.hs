{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Grapher.Vector2F
  ( Vector2F((:+))

  , zero
  , (.*)
  , (/.)
  , quadrance
  , qd
  ) where

import Data.Vector.Unboxed.Deriving

data Vector2F = !Float :+ !Float
  deriving Show

infixr 7 :+

derivingUnbox "Vector2F"
  [t| Vector2F -> (Float, Float) |]
  [| \(x:+y) -> (x,y) |]
  [| \(x,y) -> (x:+y) |]

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

quadrance :: Vector2F -> Float
quadrance (x :+ y) = x*x + y*y

qd :: Vector2F -> Vector2F -> Float
qd a b = quadrance (a - b)
