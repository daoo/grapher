module ForceGraph.World where

import ForceGraph.Ball
import ForceGraph.Rectangle
import ForceGraph.Types

data World = World
  { worldBoundary :: Rectangle
  , worldBalls :: [Ball]
  , worldLinks :: [Link]
  } deriving Show

mapBalls :: ([Ball] -> [Ball]) -> World -> World
mapBalls f w = w { worldBalls = f (worldBalls w) }

setBoundary :: Rectangle -> World -> World
setBoundary r w = w { worldBoundary = r }

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Double -> World -> World
iteration t w = id
  $ mapBalls (map $ limitRect (worldBoundary w))
  $ mapBalls (map $ integrate t)
  $ w
