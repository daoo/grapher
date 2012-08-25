module ForceGraph.Physics where

import Data.Vector2
import ForceGraph.Ball
import Math.Algebra

dragConst, repelConst :: Double
dragConst = 100.0
repelConst = 10.0

-- F = G * m1 * m2 / r^2
repel :: Ball -> Ball -> Vector2D
repel x y = f `mult` normalize (pos x - pos y)
  where
    f = (repelConst * charge x * charge y) `divZero` distSquared (pos x) (pos y)

-- F = r^2 / (D * m1 * m2)
drag :: Ball -> Ball -> Vector2D
drag x y = f `mult` normalize (pos y - pos x)
  where
    f = distSquared (pos x) (pos y) * invMass x * invMass y / dragConst 
