module Grapher.Physics where

import Grapher.Types
import Grapher.Utility
import Grapher.Vector2F

-- |Calculate the spring attraction force from one point to another.
-- Based on Hooke's law
hookes :: Float -> Point -> Point -> Force
hookes k p1 p2 = k .* (p1 - p2)

-- |Calculate the force a particle exerts on another particle.
-- The force (for a positive constant) will be directed from point 2 towards
-- point 1.
--
-- Based on Coulombs and Newtons laws.
interaction :: Float          -- ^ Constant factor
            -> (Point, Float) -- ^ Point 1
            -> (Point, Float) -- ^ Point 2
            -> Force
interaction c (p1, v1) (p2, v2) = (c * v1 * v2 `divZero` mag2 u) .* u
  where u = p1 - p2
