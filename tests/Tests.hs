module Tests where

import ForceGraph.Ball
import ForceGraph.Extensions
import ForceGraph.Physics
import ForceGraph.World
import Math.Vector2
import Test.QuickCheck

propTimes :: NonNegative Int -> Bool
propTimes (NonNegative i) = times (+1) 0 i == i

object1, object2, object3 :: Ball
object1 = Ball (Vector2 100 100) (Vector2 1 1) 10 1 100
object2 = Ball (Vector2 200 200) (Vector2 0 1) 10 1 100
object3 = Ball (Vector2 200 200) (Vector2 0 1) 10 0 100

world1, world2 :: World
world1 = World (Vector2 100 100) 500 [object1, object2]
world2 = World (Vector2 100 100) 500 [object1, object2, object3]

floatEq :: (Fractional f, Ord f) => f -> f -> Bool
floatEq a b = abs (a - b) < 0.0001

vector2Eq :: (Fractional f, Ord f) => Vector2 f -> Vector2 f -> Bool
vector2Eq a b = magSquared (a - b) < 0.0001

internalForces :: [Ball] -> [Vector2 Double]
internalForces = mapCombinations drag

propInternalForces :: NonEmptyList Ball -> Bool
propInternalForces (NonEmpty xs) =
  vector2Eq zero $ sum $ internalForces xs

propInternalForces2 :: World -> Property
propInternalForces2 w = forAll (choose (0, 10)) $
  \i -> vector2Eq zero $ sum $ internalForces $ worldBalls $ times (iteration 1) i w
