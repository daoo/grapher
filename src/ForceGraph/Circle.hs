module ForceGraph.Circle where

import Control.Applicative
import Math.Vector2
import Test.QuickCheck

data Circle = Circle
  { circlePos :: Vector2D
  , circleRadius :: Double
  }
  deriving Show

instance Arbitrary Circle where
  arbitrary = liftA2 Circle arbitrary arbitrary
