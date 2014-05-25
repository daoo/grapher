module Grapher.World
  ( World()
  , newWorld
  , ballList
  , linkList

  , iteration
  ) where

import Data.Array (Array, elems, listArray)
import Data.Array.Base (unsafeAt)
import Data.Function
import Grapher.AdjacencyMatrix
import Grapher.Particle
import Grapher.Physics
import Grapher.Types
import Grapher.Utility
import Grapher.Vector2F

repellConstant, springConstant, airDragConstant :: Float
repellConstant  = -100
springConstant  = 10
airDragConstant = 500

data World = World
  { vector :: Array Int Particle
  , matrix :: Matrix
  } deriving Show

ballAt :: World -> Int -> Particle
ballAt w i = vector w `unsafeAt` i

linked :: World -> Int -> Int -> Bool
linked = isAdjacent . matrix

ballList :: World -> [Vector2F]
ballList = map pos . elems . vector

linkList :: (Vector2F -> Vector2F -> a) -> World -> [a]
linkList f w = withAdjacent (f `on` (pos . ballAt w)) (matrix w)

newWorld :: [Particle] -> [Link] -> World
newWorld balls links = World (listArray (0, n-1) balls) (newMatrix n links)
  where
    n = length balls

iteration :: Float -> World -> World
iteration delta w = w { vector = amap (integrate delta .$. upd) $ vector w }
  where
    upd i b = force (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces w i bi = airDrag bi + center bi + aixfold f zero (vector w)
  where
    f acc j bj = acc+f1+f2
      where
        f1 = if linked w i j then attract bi bj else zero
        f2 = repell bi bj

airDrag :: Particle -> Force
airDrag ball = negate (airDragConstant .* vel ball)

center :: Particle -> Force
center = negate . pos

repell :: Particle -> Particle -> Force
repell this other = interaction repellConstant (f other) (f this)
  where
    f x = (pos x, charge x)

attract :: Particle -> Particle -> Force
attract this other = hookes springConstant (pos other) (pos this)
