module Physics where

import Math.Algebra
import Math.Vector2

import Object

maxDist, repelConst :: Double
maxDist = 100.0
repelConst = 100.0

-- | F = G * m1 * m2 / r^2
force :: Object -> Object -> Vector2D
force x y = f `mult` normalize (pos x - pos y)
  where
    f = (repelConst * charge x * charge y) `divZero` distSquared (pos x) (pos y)

-- |Calculate and apply the force that object b exerts on object a
-- F = m * a => a = F / m
repel :: Object -> Object -> Object
repel x y = x { vel = vel x + a }
  where
    f = force x y
    a = invMass x `mult` f
