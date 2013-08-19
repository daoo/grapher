module ForceGraph.Defaults (defaultWorld) where

import ForceGraph.Ball
import ForceGraph.Particle
import ForceGraph.Types
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
    [ Ball (Particle zero zero zero) 15 10000000 (-100)
    , defaultBall (Vector2 150 200) (Vector2 1 0) 10
    , defaultBall (Vector2 200 250) (Vector2 0 1) 10
    , defaultBall (Vector2 100 540) zero 10
    , defaultBall (Vector2 500 540) zero 10
    , defaultBall (Vector2 510 570) zero 10
    , defaultBall (Vector2 520 540) zero 10
    , defaultBall (Vector2 530 530) zero 10
    , defaultBall (Vector2 540 520) zero 10
    ]
  , worldLinks =
    [ (0, 1)
    , (0, 2)
    , (0, 7)
    , (0, 8)
    , (1, 3)
    , (3, 4)
    , (4, 5)
    , (5, 6)
    ]
  }
