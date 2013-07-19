module ForceGraph.Physics where

import ForceGraph.Types
import Math.Algebra
import Math.Vector2

-- |Calculate the attriactive force that the first object exercises on the
-- second object.
--
-- An object is represented by a point and a constant. The constant can be mass
-- or charge. Given the correct constant one can for example calculate either
-- the gravitational attraction between two masses or the repellent force
-- between two equal charges.
--
-- The formula used is:
--
--    F = c * a * b / r^2
--
-- Where k is the constant, a and b the values for the objects and r the
-- distance between the points.
force :: Double -> (Point, Double) -> (Point, Double) -> Force
force c (p1, v1) (p2, v2) = f `mult` normalize (p1 - p2)
  where
    f = (c * v1 * v2) `divZero` distSquared p1 p2
