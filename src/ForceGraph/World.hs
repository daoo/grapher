{-# LANGUAGE BangPatterns #-}
module ForceGraph.World
  ( World(..)
  , iteration
  ) where

import ForceGraph.Ball
import ForceGraph.EdgeMatrix
import ForceGraph.Types
import ForceGraph.Utility
import ForceGraph.Vector2D
import qualified Data.Vector.Unboxed as V

repellConstant, springConstant, airDragConstant :: Double
repellConstant  = -1000
springConstant  = 1
airDragConstant = 3

data World = World
  { worldBalls :: ![Ball]
  , worldLinks :: Matrix
  }

mapBalls :: ([Ball] -> [Ball]) -> World -> World
mapBalls f w = w { worldBalls = f (worldBalls w) }

iteration :: Double -> World -> World
iteration delta w@(World balls links) = w { worldBalls = map (integrate delta) $ mapIndex upd balls }
  where
    bc = length balls

    upd i b  = setForce (calc i b) b
    calc i b = sum $ forces balls links b i

{-# RULES "sum/map" forall f xs. sum (map f xs) = foldl (\acc x -> acc + f x) 0 xs #-}
{-# RULES "sum/cons" forall x xs. sum (x : xs) = x + sum xs #-}

forces :: [Ball] -> Matrix -> Int -> Ball -> [Force]
forces bs m b = [airDrag b, center b]
  ++ map (attract ball) bs
  ++ map (repell ball) br

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
