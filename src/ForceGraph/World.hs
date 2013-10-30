{-# LANGUAGE BangPatterns #-}
module ForceGraph.World
  ( World(..)
  , iteration
  , forces
  ) where

import Data.Array
import ForceGraph.Ball
import ForceGraph.Types
import ForceGraph.Utility
import ForceGraph.Vector2D

repellConstant, springConstant, airDragConstant :: Double
repellConstant  = -1000
springConstant  = 1
airDragConstant = 3

data World = World
  { worldBalls :: ![Ball]
  , worldLinks :: Array Int Link
  } deriving Show

mapBalls :: ([Ball] -> [Ball]) -> World -> World
mapBalls f w = w { worldBalls = f (worldBalls w) }

iteration :: Double -> World -> World
iteration delta world =
  mapBalls (map $ integrate delta) $
  mapBalls (mapIndex $ update world) world

update :: World -> Int -> Ball -> Ball
update world !i ball = setForce (sum $ forces balls linked ball) ball
  where
    balls  = worldBalls world
    links  = worldLinks world
    linked = help (elems links)

    help []                        = []
    help ((j, k) : xs) | i == j    = balls !! k : help xs
                       | i == k    = balls !! j : help xs
                       | otherwise = help xs

forces :: [Ball] -> [Ball] -> Ball -> [Force]
forces br bs ball = [ airDrag ball, center ball ]
  ++ map (repell ball) br
  ++ map (attract ball) bs

airDrag :: Ball -> Force
airDrag ball = negate ((airDragConstant * radius ball) .* velocity ball)

center :: Ball -> Force
center ball = negate p
  where
    p = position ball

repell :: Ball -> Ball -> Force
repell this other = interaction repellConstant (f other) (f this)
  where
    f x = (position x, charge x)

attract :: Ball -> Ball -> Force
attract this other = hookes springConstant (position other) (position this)

-- |Calculate the spring attraction force from one point to another.
-- Based on Hooke's law
hookes :: Double -> Point -> Point -> Force
hookes k p1 p2 = (k * d) .* n
  where
    n = normalize (p1 - p2)
    d = dist p1 p2

-- |Calculate the force a particle exerts on another particle.
-- The force (for a positive constant) will be directed from point 2 towards
-- point 1.
--
-- Based on Coulombs and Newtons laws.
interaction :: Double          -- ^ Constant factor
            -> (Point, Double) -- ^ Point 1
            -> (Point, Double) -- ^ Point 2
            -> Force
interaction c (p1, v1) (p2, v2) = f .* n
  where
    f = (c * v1 * v2) `divZero` dist p1 p2
    n = normalize (p1 - p2)
