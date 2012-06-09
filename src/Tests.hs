module Tests where

import Test.QuickCheck
import World

import Math.Vector2

object1, object2 :: Object
object1 = Object (Vector2 100 100) (Vector2 1 1) 1 100
object2 = Object (Vector2 200 200) (Vector2 0 1) 1 100
object3 = Object (Vector2 200 200) (Vector2 0 1) 0 100

world1 :: World
world1 = World [object1, object2] [(0, 1)]
