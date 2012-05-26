module Main where

import Data.Graph
import Vector2
import World

main = do
  print $ iteration 1 defaultGraph defaultObjs

defaultObjs :: [Object]
defaultObjs =
  [ Object 0 zero zero 0
  , Object 1 (Vector2 0 1) zero 1
  , Object 2 (Vector2 1 0) zero 1
  , Object 3 (Vector2 1 1) zero 1 ]

defaultGraph :: Graph
defaultGraph = buildG (0, 3)
  [ (0, 1)
  , (1, 2)
  , (2, 3)
  , (3, 0)
  ]

