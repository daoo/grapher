module ForceGraph.World where

import ForceGraph.Ball
import ForceGraph.Physics
import ForceGraph.Types
import ForceGraph.Utility
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
iteration delta world = id
                      $ mapBalls (map (integrate delta))
                      $ mapBalls (map (\ball -> setForce (forces world ball) ball))
                      $ world

forces :: World -> Ball -> Force
forces world ball = maxForce $
  repell (worldBalls world) ball +
  center ball

center :: Ball -> Force
center ball = negate p ./ 10
  where
    p = position ball

repell :: [Ball] -> Ball -> Force
repell balls ball = sum $ map (\y -> force 1000 (f y) (f ball)) balls
  where
    f x = (position x, charge x)

maxForce :: Force -> Force
maxForce =  vmap (clamp (-200) 200)
