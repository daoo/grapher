module ForceGraph.Particle where

import ForceGraph.Types
import Math.Vector2

data Particle = Particle
  { x1 :: Point
  , x2  :: Point
  , accel :: Vector2D
  } deriving Show

integrate :: Double -> Particle -> Particle
integrate t p = Particle next (x1 p) (accel p)
  where
    next = x1 p + (x1 p - x2 p) + (t * t) .* accel p
