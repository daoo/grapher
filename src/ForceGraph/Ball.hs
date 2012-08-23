module ForceGraph.Ball where

import Control.Monad
import ForceGraph.Time
import Math.Vector2
import Test.QuickCheck

type Mass     = Double
type Charge   = Double
type Radius   = Double
type Point    = Vector2D
type Velocity = Vector2D

data Ball = Ball
  { pos :: Point
  , vel :: Velocity
  , radius :: Radius
  , invMass :: Mass
  , charge :: Charge
  } deriving (Show, Eq)

instance Arbitrary Ball where
  arbitrary = liftM5 Ball arbitrary arbitrary arbitrary arbitrary arbitrary

  shrink (Ball p v r i c) =
    [Ball p' v' r i' c' | p' <- shrink p, v' <- shrink v, i' <- shrink i, c' <- shrink c]

integrate :: Time -> Ball -> Ball
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

addVel :: Ball -> Velocity -> Ball
addVel b v = b { vel = vel b + v }

setVel :: Ball -> Velocity -> Ball
setVel b v = b { vel = v }
