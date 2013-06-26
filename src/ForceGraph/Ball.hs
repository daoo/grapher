module ForceGraph.Ball where

import Control.Applicative
import ForceGraph.Physics
import Test.QuickCheck.Arbitrary

type Radius = Double

data Ball = Ball
  { pos :: Point
  , pos' :: Point
  , radius :: Radius
  , mass :: Mass
  , charge :: Charge
  } deriving (Show, Eq)

instance Arbitrary Ball where
  arbitrary = Ball <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (Ball p v r i c) =
    [Ball p' v' r i' c' | p' <- shrink p, v' <- shrink v, i' <- shrink i, c' <- shrink c]

integrate :: Double -> Ball -> Ball
integrate _ obj = obj { pos = p + (p - p'), pos' = p }
  where
    p = pos obj
    p' = pos' obj
