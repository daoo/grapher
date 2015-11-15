module Grapher.Physics where

import Linear.Metric
import Linear.V2

-- |Calculate the spring attraction force from one point to another.
--
-- Based on Hooke's law.
hookes :: Float -> V2 Float -> V2 Float -> V2 Float
hookes k p1 p2 = realToFrac k * (p1 - p2)

-- |Calculate the force a particle exerts on another particle.
-- The force (for a positive constant) will be directed from point 2 towards
-- point 1.
--
-- Based on Coulombs and Newtons laws.
interaction :: Float -- ^ Constant factor
            -> (V2 Float, Float) -- ^ Point 1
            -> (V2 Float, Float) -- ^ Point 2
            -> V2 Float
interaction c (p1, v1) (p2, v2) = realToFrac (c * v1 * v2 / quadrance u) * u
  where u = p1 - p2
