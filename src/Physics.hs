module Physics where

import Math.Algebra
import Math.Vector2

import Object

maxDist, chargeConstant :: Double
maxDist = 100.0
chargeConstant = 100.0

force :: Object -> Object -> Double
force a b = r * c
  where
    r = invMass a `divZero` (invMass a + invMass b)
    c = (chargeConstant * charge a * charge b) `divZero` distSquared (pos a) (pos b)

-- |Calculate and apply the force that object b exerts on object a
repel :: Object -> Object -> Object
repel a b = a { vel = vel a + v }
  where
    f = force a b
    d = normalize $ pos a - pos b
    v = f `mult` d

-- |Constraint movement between two objects
constraint :: Object -> Object -> Object
constraint a b | mag d > maxDist = a { vel = v }
               | otherwise       = a
  where
    d = pos b - pos a
    p = normalize $ orthogonal d
    s = mag (vel a)
    v = s `mult` p
