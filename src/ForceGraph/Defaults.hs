module ForceGraph.Defaults
  ( defaultWorld ) where

import ForceGraph.Ball
import ForceGraph.Particle
import ForceGraph.Types
import ForceGraph.Utility
import ForceGraph.World
import Math.Vector2

defaultBall :: Point -> Velocity -> Radius -> Ball
defaultBall p v r = Ball
  { particle = Particle (p + v) p zero
  , radius   = r
  , mass     = 1
  , charge   = 10
  }

defaultWorld :: World
defaultWorld = World
  { worldBalls =
    [ Ball (Particle zero zero zero) 15 10000000 10
    , defaultBall (Vector2D 150 200) zero 10
    , defaultBall (Vector2D 200 250) zero 10
    , defaultBall (Vector2D 100 540) zero 10
    , defaultBall (Vector2D 500 540) zero 10
    , defaultBall (Vector2D 510 570) zero 10
    , defaultBall (Vector2D 520 540) zero 10
    , defaultBall (Vector2D 530 530) zero 10
    , defaultBall (Vector2D 540 520) zero 10
    , defaultBall (Vector2D 300 300) zero 10
    , defaultBall (Vector2D 600 520) zero 10
    ]
  , worldLinks = arrayList
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
  }
