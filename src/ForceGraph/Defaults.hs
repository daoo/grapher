module ForceGraph.Defaults where

import ForceGraph.Ball
import ForceGraph.Particle
import ForceGraph.Rectangle
import ForceGraph.Types
import ForceGraph.World
import Math.Vector2

defaultBall :: Point -> Velocity -> Radius -> Mass -> Ball
defaultBall p v r m = Ball (Particle (p + v) p zero) r m 100

defaultWorld :: World
defaultWorld = World
  { worldBoundary = Rectangle 1920 1080
  , worldBalls =
    [ defaultBall (Vector2 0 0) zero 20 10
    , defaultBall (Vector2 150 200) (Vector2 1 0) 10 1
    , defaultBall (Vector2 200 250) (Vector2 0 1) 10 1
    , defaultBall (Vector2 100 540) zero 10 1
    , defaultBall (Vector2 500 540) zero 10 1
    , defaultBall (Vector2 510 570) zero 10 1
    , defaultBall (Vector2 520 540) zero 10 1
    , defaultBall (Vector2 530 530) zero 10 1
    , defaultBall (Vector2 540 520) zero 10 1
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
