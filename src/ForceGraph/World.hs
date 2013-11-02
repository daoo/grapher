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
import ForceGraph.Vector2F
import qualified Data.Array as A

repellConstant, springConstant, airDragConstant :: Float
repellConstant  = -100
springConstant  = 10
airDragConstant = 30

data World = World
  { vector :: Array Int Ball
  , matrix :: Matrix
  } deriving Show

ballCount :: World -> Int
ballCount = (\(a, b) -> b - a) . A.bounds . vector

ballAt :: World -> Int -> Ball
ballAt w i = assert (i < ballCount w) $
  vector w `unsafeAt` i

linked :: World -> Int -> Int -> Bool
linked = isLinked . matrix

ballMap :: (Ball -> a) -> World -> [a]
ballMap f = map f . A.elems . vector

linkBalls :: (Ball -> Ball -> a) -> World -> [a]
linkBalls f w = withLinked (ballAt w) f (matrix w)

newWorld :: [Ball] -> [Link] -> World
newWorld balls links = World (alist balls) (newMatrix (length balls) links)

iteration :: Float -> World -> World
iteration delta w = w { vector = amap (integrate delta .$. upd) $ vector w }
  where
    upd i b = setForce (forces w i b) b

forces :: World -> Int -> Ball -> Force
forces w i bi = airDrag bi + center bi + aixfold f zero (vector w)
  where
    f acc j bj = acc+f1+f2
      where
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
hookes :: Float -> Point -> Point -> Force
hookes k p1 p2 = (k * d) .* n
  where
    n = normalize (p1 - p2)
    d = dist p1 p2

-- |Calculate the force a particle exerts on another particle.
-- The force (for a positive constant) will be directed from point 2 towards
-- point 1.
--
-- Based on Coulombs and Newtons laws.
interaction :: Float          -- ^ Constant factor
            -> (Point, Float) -- ^ Point 1
            -> (Point, Float) -- ^ Point 2
            -> Force
interaction c (p1, v1) (p2, v2) = f .* n
  where
    f = (c * v1 * v2) `divZero` dist p1 p2
    n = normalize (p1 - p2)
