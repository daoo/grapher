module Grapher.Physics where

import Grapher.Vector2F

-- |Calculate the spring attraction force from one point to another.
--
-- Based on Hooke's law.
hookes :: Float -> Vector2F -> Vector2F -> Vector2F
hookes k p1 p2 = k .* (p1 - p2)

-- |Calculate the force a particle exerts on another particle.
-- The force (for a positive constant) will be directed from point 2 towards
-- point 1.
--
-- Based on Coulombs and Newtons laws.
interaction :: Float -- ^ Constant factor
            -> (Vector2F, Float) -- ^ Point 1
            -> (Vector2F, Float) -- ^ Point 2
            -> Vector2F
interaction c (p1, v1) (p2, v2) = (c * v1 * v2 / quadrance u) .* u
  where u = p1 - p2
