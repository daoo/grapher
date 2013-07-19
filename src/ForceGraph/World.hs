module ForceGraph.World where

import ForceGraph.Ball
import ForceGraph.Physics
import ForceGraph.Types
import Math.Vector2

data World = World
  { worldBalls :: [Ball]
  , worldLinks :: [Link]
  } deriving Show

mapBalls :: ([Ball] -> [Ball]) -> World -> World
mapBalls f w = w { worldBalls = f (worldBalls w) }

showWorld :: World -> String
showWorld = unlines . map show . worldBalls

iteration :: Double -> World -> World
iteration t w = id
  $ mapBalls (map $ forces w)
  $ mapBalls (map $ integrate t)
  $ w

forces :: World -> Ball -> Ball
forces w b = setForce (repell (worldBalls w) b + centerPull b) b

repell :: [Ball] -> Ball -> Force
repell bs b = sum $ map (force 1 (f b) . f) bs
  where
    f x = (position x, charge x)

centerPull :: Ball -> Force
centerPull b = force 20 (zero, 1) (position b, mass b)
