module ForceGraph.Types
  ( Charge
  , Force
  , Mass
  , Velocity
  , Accel
  , Point
  , Radius
  , Link
  ) where

import ForceGraph.Vector2F

type Charge   = Float
type Force    = Vector2F
type Mass     = Float
type Velocity = Vector2F
type Accel    = Vector2F

type Point  = Vector2F
type Radius = Float

type Link = (Int, Int)
