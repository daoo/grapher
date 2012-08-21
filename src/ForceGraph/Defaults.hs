module ForceGraph.Defaults where

import ForceGraph.Ball
import ForceGraph.World
import Math.Vector2

defaultWorld :: World
defaultWorld = World
  { worldLimitPos = Vector2 500 500
  , worldLimitRadius = 490
  , worldBalls =
    [ Ball (Vector2 250 250) zero 10 0 100
    , Ball (Vector2 150 200) zero 10 1 100
    , Ball (Vector2 200 250) zero 10 1 100
    , Ball (Vector2 100 540) zero 10 1 100
    , Ball (Vector2 500 540) zero 10 1 100
    ]
  }
