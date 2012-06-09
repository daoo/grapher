module Object where

import Math.Vector2
import Test.QuickCheck

type Mass     = Double
type Charge   = Double
type Point    = Vector2D
type Velocity = Vector2D

data Object = Object
  { pos :: Point
  , vel :: Velocity
  , invMass :: Mass
  , charge :: Charge }
  deriving (Show, Eq)

instance Arbitrary Object where
  arbitrary = do
    p <- arbitrary
    v <- arbitrary
    m <- arbitrary
    c <- arbitrary
    return $ Object p v m c

  shrink (Object p v i c) =
    [Object p' v' i' c' | p' <- shrink p, v' <- shrink v, i' <- shrink i, c' <- shrink c]
