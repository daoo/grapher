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

import ForceGraph.Vector2D

type Charge   = Double
type Force    = Vector2D
type Mass     = Double
type Velocity = Vector2D
type Accel    = Vector2D

type Point  = Vector2D
type Radius = Double

type Link = (Int, Int)
