module ForceGraph.Particle
  ( Particle
  , mkParticle
  , pos
  , vel
  , integrate
  , force
  ) where

import ForceGraph.Types
import ForceGraph.Vector2F

data Particle = Particle
  { x1    :: {-# UNPACK #-} !Point
  , x2    :: {-# UNPACK #-} !Point
  , mass  :: {-# UNPACK #-} !Mass
  , accel :: {-# UNPACK #-} !Vector2F
  } deriving Show

{-# INLINE mkParticle #-}
mkParticle :: Vector2F -> Float -> Particle
mkParticle p m = Particle p p m zero

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
