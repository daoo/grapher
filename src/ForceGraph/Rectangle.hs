module ForceGraph.Rectangle where

import Control.Applicative
import Test.QuickCheck

data Rectangle = Rectangle
  { width :: Double
  , height :: Double
  } deriving Show

instance Arbitrary Rectangle where
  arbitrary = Rectangle <$> f <*> f
    where f = choose (100, 10000)
