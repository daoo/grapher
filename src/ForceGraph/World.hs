module ForceGraph.World where

import ForceGraph.Ball
import ForceGraph.Extensions
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
iteration t w = mapBalls (map $ (\b -> setForce (forces w b) b))
              $ mapBalls (map $ integrate t) w

forces :: World -> Ball -> Force
forces w b =
  airDrag b +
  maxForce (repell (worldBalls w) b)

repell :: [Ball] -> Ball -> Force
repell bs b = sum $ map (\y -> force 1000 (f y) (f b)) bs
  where
    f x = (position x, charge x)

maxForce :: Force -> Force
maxForce (Vector2 x y) = Vector2 (f x) (f y)
  where f = clamp (-200) 200

airDrag :: Ball -> Force
airDrag b = m .* d
  where
    m = magSquared v
    d = normalize v
    v = velocity b
