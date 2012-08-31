module ForceGraph.Physics where

import Data.Vector2
import ForceGraph.Ball
import Math.Algebra

type Force = Vector2D

repelConst :: Double
repelConst = 1000.0

-- F = k * c1 * c2 / r^2
repel :: Ball -> Ball -> Force
repel a b = f `mult` normalize (pos a - pos b)
  where
    f = (repelConst * charge a * charge b) `divZero` distSquared (pos a) (pos b)
