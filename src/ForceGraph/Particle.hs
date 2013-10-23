module ForceGraph.Particle where

import ForceGraph.Types
import Math.Vector2

data Particle = Particle
  { x1 :: Point
  , x2  :: Point
  , accel :: Vector2D
  } deriving Show

integrate :: Double -> Particle -> Particle
integrate t p = p { x1 = next, x2 = x1 p }
  where
    next = x1 p + (x1 p - x2 p) + (t * t) .* accel p
