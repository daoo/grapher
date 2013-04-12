module ForceGraph.Circle where

import Control.Applicative
import Data.Vector2
import Test.QuickCheck

data Circle = Circle
  { pos :: Vector2D
  , radius :: Double
  } deriving Show

instance Arbitrary Circle where
  arbitrary = Circle <$> arbitrary <*> arbitrary
