module ForceGraph.Defaults where

import ForceGraph.Object
import ForceGraph.World
import Math.Vector2

defaultWorld :: World
defaultWorld = World
  { worldObjects =
    [ Object (Vector2 250 250) zero 0 10
    , Object (Vector2 100 200) zero 1 10
    , Object (Vector2 200 250) zero 1 10
    , Object (Vector2 100 540) zero 1 10
    , Object (Vector2 500 540) zero 1 10
    ]
  , worldRopes =
    [ (0, 1)
    , (0, 2)
    , (2, 3)
    , (3, 4)
    ]
  }
