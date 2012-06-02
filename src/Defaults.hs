module Defaults where

import Math.Vector2
import World

defaultWorld1 :: World
defaultWorld1 = World
  { worldObjects = 
    [ Object (Vector2 250 250) zero 0
    , Object (Vector2 100 200) zero 1
    , Object (Vector2 200 250) zero 1
    , Object (Vector2 100 540) zero 1
    ]
  , worldConnections =
    [ (0, 1)
    , (0, 2)
    , (2, 3)
    ]
  }

defaultWorld :: World
defaultWorld = World
  { worldObjects = 
    [ Object (Vector2 100 250) zero 0
    , Object (Vector2 200 250) zero 1
    ]
  , worldConnections =
    [ (0, 1) ]
  }
