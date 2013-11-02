module ForceGraph.Particle
  ( Particle(..)
  , integrate
  ) where

import ForceGraph.Types
import ForceGraph.Vector2F

data Particle = Particle
  { x1    :: {-# UNPACK #-} !Point
  , x2    :: {-# UNPACK #-} !Point
  , accel :: {-# UNPACK #-} !Vector2F
  } deriving Show

integrate :: Float -> Particle -> Particle
integrate t p = p { x1 = next, x2 = x1 p }
  where
    next = x1 p + (x1 p - x2 p) + (t * t) .* accel p
