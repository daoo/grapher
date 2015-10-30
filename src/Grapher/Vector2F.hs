{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
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

import Control.Monad (liftM)
import Data.Vector.Unboxed.Base
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

square :: Float -> Float
square x = x * x

data Vector2F = !Float :+ !Float
  deriving Show

newtype instance MVector s Vector2F = MV_Vector2F (MVector s Float)
newtype instance Vector    Vector2F = V_Vector2F  (Vector    Float)
instance Unbox Vector2F

instance M.MVector MVector Vector2F where
  {-# INLINE basicLength #-}
  basicLength (MV_Vector2F v) = M.basicLength v `quot` 2

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice a b (MV_Vector2F v) = MV_Vector2F $ M.basicUnsafeSlice (a*2) (b*2) v

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MV_Vector2F v0) (MV_Vector2F v1) = M.basicOverlaps v0 v1

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = liftM MV_Vector2F (M.basicUnsafeNew (2*n))

  basicInitialize (MV_Vector2F v) = M.basicInitialize v

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MV_Vector2F v) n = do
    let n' = 2*n
    x <- M.basicUnsafeRead v n'
    y <- M.basicUnsafeRead v (n'+1)
    return (x:+y)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MV_Vector2F v) n (x:+y) = do
    let n' = 2*n
    M.basicUnsafeWrite v n'     x
    M.basicUnsafeWrite v (n'+1) y

instance G.Vector Vector Vector2F where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MV_Vector2F v) = liftM V_Vector2F (G.basicUnsafeFreeze v)

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (V_Vector2F v) = liftM MV_Vector2F (G.basicUnsafeThaw v)

  {-# INLINE basicLength #-}
  basicLength (V_Vector2F v) = G.basicLength v `quot` 2

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice a b (V_Vector2F v) = V_Vector2F $ G.basicUnsafeSlice (a*2) (b*2) v

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (V_Vector2F v) n = do
    let n' = 2*n
    x <- G.basicUnsafeIndexM v n'
    y <- G.basicUnsafeIndexM v (n'+1)
    return (x:+y)

infixr 7 :+

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
