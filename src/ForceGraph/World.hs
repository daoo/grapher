{-# LANGUAGE BangPatterns #-}
module ForceGraph.World
  ( World()
  , ballMap
  , linkBalls
  , newWorld
  , iteration
  ) where

import Control.Exception
import Data.Array (Array)
import Data.Array.Base (unsafeAt)
import ForceGraph.Ball
import ForceGraph.LinkMatrix
import ForceGraph.Types
import ForceGraph.Utility
import ForceGraph.Vector2D
import qualified Data.Array as A

alen :: (Num i, A.Ix i) => Array i e -> i
alen = (\(a, b) -> b - a) . A.bounds

repellConstant, springConstant, airDragConstant :: Double
repellConstant  = -1000
springConstant  = 1
airDragConstant = 3

data World = World
  { vector :: Array Int Ball
  , matrix :: Matrix
  }

ballCount :: World -> Int
ballCount = alen . vector

ballAt :: World -> Int -> Ball
ballAt w i = assert (i < ballCount w) $
  vector w `unsafeAt` i

linked :: World -> Int -> Int -> Bool
linked w i j = isLinked (matrix w) i j

ballMap :: (Ball -> a) -> World -> [a]
ballMap f = map f . A.elems . vector

linkBalls :: World -> [(Ball, Ball)]
linkBalls w = go 0 0
  where
    n = ballCount w

    go !i !j | j < n     = if linked w i j then (w `ballAt` i, w `ballAt` j) : go i (j+1) else go i (j+1)
             | i < n     = go (i+1) i
             | otherwise = []

newWorld :: [Ball] -> [Link] -> World
newWorld balls links = World v (newMatrix n links)
  where
    v = A.listArray (0, length balls - 1) balls
    n = alen v

iteration :: Double -> World -> World
iteration delta w = w { vector = A.listArray (A.bounds $ vector w) $ map (integrate delta . upd) $ A.assocs $ vector w }
  where
    upd (i, b) = setForce (forces w i b) b

forces :: World -> Int -> Ball -> Force
forces w i bi = airDrag bi + center bi + go zero 0
  where
    go !acc !j | j < ballCount w = go (acc+f1+f2) (j+1)
               | otherwise       = acc

      where
        bj = w `ballAt` j

        f1 = if linked w i j then attract bi bj else zero
        f2 = repell bi bj

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
