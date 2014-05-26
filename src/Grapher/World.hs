module Grapher.World
  ( World()
  , newWorld
  , particleList
  , linkList

  , iteration
  ) where

import Data.Function
import Data.Vector (Vector)
import qualified Data.Vector as V
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
  { vector :: Vector Particle
  , matrix :: Matrix
  } deriving Show

particle :: World -> Int -> Particle
particle w i = vector w `V.unsafeIndex` i

linked :: World -> Int -> Int -> Bool
linked = isAdjacent . matrix

particleList :: World -> [Vector2F]
particleList = map pos . V.toList . vector

linkList :: (Vector2F -> Vector2F -> a) -> World -> [a]
linkList f w = withAdjacent (f `on` (pos . particle w)) (matrix w)

newWorld :: [Particle] -> [Link] -> World
newWorld parts links = World (V.fromListN n parts) (newMatrix n links)
  where
    n = length parts

iteration :: Float -> World -> World
iteration delta w = w { vector = V.imap (integrate delta .$. upd) $ vector w }
  where
    upd i b = force (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces w i bi = airDrag bi + center bi + V.ifoldl' f zero (vector w)
  where
    f acc j bj = acc+f1+f2
      where
        f1 = if linked w i j then attract bi bj else zero
        f2 = repell bi bj

airDrag :: Particle -> Force
airDrag p = negate (airDragConstant .* vel p)

center :: Particle -> Force
center = negate . pos

repell :: Particle -> Particle -> Force
repell this other = interaction repellConstant (f other) (f this)
  where
    f x = (pos x, charge x)

attract :: Particle -> Particle -> Force
attract this other = hookes springConstant (pos other) (pos this)
