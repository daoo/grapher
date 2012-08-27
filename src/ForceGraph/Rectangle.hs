module ForceGraph.Rectangle where

import Control.Applicative
import Test.QuickCheck

data Rectangle = Rectangle
  { rectWidth :: Double
  , rectHeight :: Double
  }
  deriving Show

instance Arbitrary Rectangle where
  arbitrary = liftA2 Rectangle f f
    where f = choose (100, 10000)
