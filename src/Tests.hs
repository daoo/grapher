module Tests where

import Extensions
import Test.QuickCheck

import Math.Vector2

import Object
import Physics
import World

object1, object2, object3 :: Object
object1 = Object (Vector2 100 100) (Vector2 1 1) 1 100
object2 = Object (Vector2 200 200) (Vector2 0 1) 1 100
object3 = Object (Vector2 200 200) (Vector2 0 1) 0 100

world1, world2 :: World
world1 = World [object1, object2] [(0, 1)]
world2 = World [object1, object2, object3] [(0, 1), (1, 2)]

floatEq :: (Fractional f, Ord f) => f -> f -> Bool
floatEq a b = abs (a - b) < 0.0001

vector2Eq :: (Fractional f, Ord f) => Vector2 f -> Vector2 f -> Bool
vector2Eq a b = magSquared (a - b) < 0.0001

internalForces :: [Object] -> [Vector2 Double]
internalForces xs = mapCombinations (\a b -> force a b `mult` (normalize $ pos b - pos a)) xs

propInternalForcesEqualsZero :: NonEmptyList Object -> Bool
propInternalForcesEqualsZero (NonEmpty xs) =
  (vector2Eq zero) $ sum $ internalForces xs
