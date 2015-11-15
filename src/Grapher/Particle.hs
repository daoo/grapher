{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Grapher.Particle
  ( Particle
  , unsafeParticle
  , fromPoint
  , pos
  , vel
  , integrate
  , force
  ) where

import Data.Vector.Unboxed.Deriving
import Linear.V2
import Linear.Vector

data Particle = Particle
  { x1     :: !(V2 Float)
  , x2     :: !(V2 Float)
  , accel  :: !(V2 Float)
  } deriving Show

derivingUnbox "Particle"
  [t| Particle -> (V2 Float, V2 Float, V2 Float) |]
  [| \p -> (x1 p, x2 p, accel p) |]
  [| \(a, b, c) -> Particle a b c |]

{-# INLINE fromPoint #-}
fromPoint :: V2 Float -> Particle
fromPoint p = Particle p p zero

{-# INLINE unsafeParticle #-}
unsafeParticle :: V2 Float -> V2 Float -> V2 Float -> Particle
unsafeParticle = Particle

{-# INLINE pos #-}
pos :: Particle -> V2 Float
pos = x1

{-# INLINE vel #-}
vel :: Particle -> V2 Float
vel n = x1 n - x2 n

{-# INLINE move #-}
move :: Particle -> V2 Float -> Particle
move n p = n { x1 = p, x2 = x1 n }

{-# INLINE force #-}
force :: Float -> V2 Float -> Particle -> Particle
force m f p = p { accel = f / realToFrac m }

{-# INLINE integrate #-}
integrate :: Float -> Particle -> Particle
integrate t p = move p $ pos p + vel p + realToFrac (t * t) * accel p
