module ForceGraph.Defaults
  ( defaultWorld ) where

import ForceGraph.Ball
import ForceGraph.Particle
import ForceGraph.Types
import ForceGraph.Vector2D
import ForceGraph.World

defaultBall :: Point -> Velocity -> Radius -> Ball
defaultBall p v r = Ball
  { particle = Particle (p + v) p zero
  , radius   = r
  , mass     = 1
  , charge   = 10
  }

defaultWorld :: World
defaultWorld = newWorld
  [ Ball (Particle zero zero zero) 15 10000000 10
  , defaultBall (150 .+ 200) zero 10
  , defaultBall (200 .+ 250) zero 10
  , defaultBall (100 .+ 540) zero 10
  , defaultBall (500 .+ 540) zero 10
  , defaultBall (510 .+ 570) zero 10
  , defaultBall (520 .+ 540) zero 10
  , defaultBall (530 .+ 530) zero 10
  , defaultBall (540 .+ 520) zero 10
  , defaultBall (300 .+ 300) zero 10
  , defaultBall (600 .+ 520) zero 10
  ]
  [ (0, 1)
  , (0, 2)
  , (0, 7)
  , (0, 8)
  , (1, 3)
  , (3, 4)
  , (4, 5)
  , (4, 9)
  , (4, 10)
  , (5, 6)
  ]
