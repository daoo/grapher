module ForceGraph.Ball where

import Control.Applicative
import Data.Vector2
import ForceGraph.Physics
import ForceGraph.Time
import Test.QuickCheck.Arbitrary

type Radius = Double

data Ball = Ball
  { pos :: Point
  , vel :: Velocity
  , radius :: Radius
  , mass :: Mass
  , charge :: Charge
  } deriving (Show, Eq)

instance Arbitrary Ball where
  arbitrary = Ball <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (Ball p v r i c) =
    [Ball p' v' r i' c' | p' <- shrink p, v' <- shrink v, i' <- shrink i, c' <- shrink c]

integrate :: Time -> Ball -> Ball
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

addVel :: Ball -> Velocity -> Ball
addVel b v = b { vel = vel b + v }

setVel :: Ball -> Velocity -> Ball
setVel b v = b { vel = v }
