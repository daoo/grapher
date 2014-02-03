module Grapher.Particle
  ( Particle
  , mkParticle
  , pos
  , vel
  , charge
  , integrate
  , force
  ) where

import Grapher.Types
import Grapher.Vector2F

data Particle = Particle
  { x1     :: {-# UNPACK #-} !Point
  , x2     :: {-# UNPACK #-} !Point
  , accel  :: {-# UNPACK #-} !Vector2F
  , mass   :: {-# UNPACK #-} !Mass
  , charge :: {-# UNPACK #-} !Charge
  } deriving Show

{-# INLINE mkParticle #-}
mkParticle :: Vector2F -> Mass -> Charge -> Particle
mkParticle p = Particle p p zero

{-# INLINE pos #-}
pos :: Particle -> Vector2F
pos = x1

{-# INLINE vel #-}
vel :: Particle -> Vector2F
vel n = x1 n - x2 n

move :: Particle -> Vector2F -> Particle
move n p = n { x1 = p, x2 = x1 n }

force :: Force -> Particle -> Particle
force f p = p { accel = f /. mass p }

integrate :: Float -> Particle -> Particle
integrate t p = move p $ pos p + vel p + (t * t) .* accel p
