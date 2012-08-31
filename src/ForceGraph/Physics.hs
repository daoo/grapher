module ForceGraph.Physics where

import Data.Vector2
import Math.Algebra

type Charge   = Double
type Force    = Vector2D
type Mass     = Double
type Point    = Vector2D
type Velocity = Vector2D

repelConst :: Double
repelConst = 1000.0

-- F = k * c1 * c2 / r^2
repel :: (Point, Charge) -> (Point, Charge) -> Force
repel (pa, ca) (pb, cb) = f `mult` normalize (pa - pb)
  where
    f = (repelConst * ca * cb) `divZero` distSquared (pa) (pb)

collision :: (Velocity, Mass) -> (Velocity, Mass) -> Velocity
collision (va, ma) (vb, mb) = (1 / (ma + mb)) `mult` p
  where
    p = ma `mult` va + mb `mult` vb
