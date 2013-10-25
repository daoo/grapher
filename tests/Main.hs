module Main (main) where

import Control.Applicative
import ForceGraph.Ball
import ForceGraph.Defaults
import ForceGraph.Particle
import ForceGraph.Utility
import ForceGraph.World
import Math.Vector2
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Vector2

instance Arbitrary Particle where
  arbitrary = do
    x1 <- arbitrary
    vx <- choose (-10, 10)
    vy <- choose (-10, 10)
    return $ Particle x1 (x1 + (vx .+ vy)) zero

instance Arbitrary Ball where
  arbitrary = Ball <$> arbitrary <*> choose (10, 50) <*> pure 1 <*> pure 10

instance Arbitrary World where
  arbitrary = World <$> arbitrary <*> pure []

  shrink (World balls links) = zipWith World (shrink balls) (shrink links)

propTimes :: NonNegative Int -> Bool
propTimes (NonNegative i) = times (+1) 0 i == i

object1, object2, object3 :: Ball
object1 = defaultBall (100 .+ 100) (1 .+ 1) 10
object2 = defaultBall (200 .+ 200) (0 .+ 1) 10
object3 = defaultBall (200 .+ 200) (0 .+ 1) 10

world1, world2 :: World
world1 = World [object1, object2] []
world2 = World [object1, object2, object3] []

floatEq :: (Fractional f, Ord f) => f -> f -> Bool
floatEq a b = abs (a - b) < 0.0001

internalForces :: [Ball] -> [Vector2 Double]
internalForces balls = map (repell balls) balls

propInternalForces :: NonEmptyList Ball -> Bool
propInternalForces (NonEmpty xs) =
  vector2Eq zero $ sum $ internalForces xs

propInternalForces2 :: World -> Property
propInternalForces2 w = forAll (choose (1, 10)) $
  \i -> vector2Eq zero $ sum $ internalForces $ worldBalls $ times (iteration 1) i w

tests :: [Test]
tests =
  [ testGroup "misc"
    [ testProperty "Times successor" propTimes ]
  , vectorTests
  , testGroup "forces"
    [ testProperty "Internal forces equals zero" propInternalForces
    , testProperty "Internal forces equals zero 2" propInternalForces2
    ]
  ]

main :: IO ()
main = defaultMain tests
