module ForceGraph.World where

import ForceGraph.Ball
import ForceGraph.Types
import ForceGraph.Utility
import Math.Algebra
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
iteration delta world =
  mapBalls (map $ integrate delta) $
  mapBalls (map $ \ball -> setForce (forces world ball) ball) world

forces :: World -> Ball -> Force
forces world ball =
  maxForce 200 (repell (worldBalls world) ball) +
  maxForce 100 (center ball)

center :: Ball -> Force
center ball = negate p
  where
    p = position ball

repell :: [Ball] -> Ball -> Force
repell balls ball = sum $ map (\y -> force 100 (f y) (f ball)) balls
  where
    f x = (position x, charge x)

maxForce :: Double -> Force -> Force
maxForce c f | m > c     = c .* normalize f
             | otherwise = f
  where
    m = mag f

force :: Double -> (Point, Double) -> (Point, Double) -> Force
force c (p1, v1) (p2, v2) = f .* n
  where
    f = (c * v1 * v2) `divZero` dist p1 p2
    n = normalize (p1 - p2)
