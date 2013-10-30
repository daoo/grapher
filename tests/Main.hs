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

propTimes :: NonNegative Int -> Bool
propTimes (NonNegative i) = times (+1) 0 i == i

object1, object2, object3 :: Ball
object1 = defaultBall (100 .+ 100) (1 .+ 1) 10
object2 = defaultBall (200 .+ 200) (0 .+ 1) 10
object3 = defaultBall (200 .+ 200) (0 .+ 1) 10

floatEq :: (Fractional f, Ord f) => f -> f -> Bool
floatEq a b = abs (a - b) < 0.0001

internalForces :: [Ball] -> [Vector2 Double]
internalForces balls = concatMap (\ball -> map (repell ball) balls) balls

propInternalForces :: NonEmptyList Ball -> Bool
propInternalForces (NonEmpty xs) =
  vector2Eq zero $ sum $ internalForces xs

tests :: [Test]
tests =
  [ testGroup "misc"
    [ testProperty "Times successor" propTimes ]
  , vectorTests
  , testGroup "forces"
    [ testProperty "Internal forces equals zero" propInternalForces
    ]
  ]

main :: IO ()
main = defaultMain tests
