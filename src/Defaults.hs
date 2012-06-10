module Defaults where

import Math.Vector2

import Object
import World

defaultWorld :: World
defaultWorld = World
  { worldObjects =
    [ Object (Vector2 250 250) zero 0 10
    , Object (Vector2 100 200) zero 1 10
    , Object (Vector2 200 250) zero 1 10
    , Object (Vector2 100 540) zero 1 10
    , Object (Vector2 500 540) zero 1 10
    ]
  , worldConnections =
    [ (0, 1)
    , (0, 2)
    , (2, 3)
    , (3, 4)
    ]
  }
