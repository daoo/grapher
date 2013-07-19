module ForceGraph.Particle where

import ForceGraph.Extensions
import ForceGraph.Rectangle
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
    next = x1 p + (x1 p - x2 p) + (t * t) `mult` accel p

limitRect :: Rectangle -> Particle -> Particle
limitRect rect p = Particle (Vector2 px' py') (x1 p) (accel p)
  where
    Vector2 px py = x1 p

    px' = clamp 0 (width rect) px
    py' = clamp 0 (height rect) py
