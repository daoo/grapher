module Defaults where

import Math.Vector2
import World

defaultObjs :: [Object]
defaultObjs =
  [ Object (Vector2 250 250) zero 0
  , Object (Vector2 100 200) zero 1
  , Object (Vector2 200 250) zero 1
  , Object (Vector2 100 540) zero 1 ]

defaultGraph :: [Connection]
defaultGraph =
  [ (0, 1)
  , (0, 2)
  , (2, 3)
  ]
