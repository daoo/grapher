module ForceGraph.Defaults where

import Data.Vector2
import ForceGraph.Ball
import ForceGraph.Rectangle
import ForceGraph.World

defaultWorld :: World
defaultWorld = World
  { worldBoundary = Rectangle 500 500
  , worldBalls =
    [ Ball (Vector2 250 250) zero 10 0 100
    , Ball (Vector2 150 200) zero 10 1 100
    , Ball (Vector2 200 250) zero 10 1 100
    , Ball (Vector2 100 540) zero 10 1 100
    , Ball (Vector2 500 540) zero 10 1 100
    , Ball (Vector2 510 570) zero 10 1 100
    , Ball (Vector2 520 540) zero 10 1 100
    , Ball (Vector2 530 530) zero 10 1 100
    , Ball (Vector2 540 520) zero 10 1 100
    ]
  }
