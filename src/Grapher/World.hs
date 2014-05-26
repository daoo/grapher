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
import Grapher.Vector2F

repellConstant, springConstant, airDragConstant :: Float
repellConstant  = -100
springConstant  = 10
airDragConstant = 500

data World = World
  { vector :: !(Vector Particle)
  , matrix :: !Matrix
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
iteration delta w = w { vector = V.imap f $ vector w }
  where
    f i b = integrate delta $ force (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces w i bi = airDrag bi + centerPull bi + V.sum (V.imap intrct (vector w))
  where
    intrct j bj
      | linked w i j = attract bi bj + repell bi bj
      | otherwise    = repell bi bj

-- |Calculate the air drag force exerted on a particle.
--
-- The force is linearly proportional to the speed of the particle.
airDrag :: Particle -> Force
airDrag p = negate (airDragConstant .* vel p)

-- |Calculates the pulling force towards the center.
--
-- The force is linearly proportional to the distance from the center.
centerPull :: Particle -> Force
centerPull = negate . pos

-- |Calcualte the repell force exerted by the first particle on the second.
repell :: Particle -> Particle -> Force
repell this other = interaction repellConstant (f other) (f this)
  where
    f x = (pos x, charge x)

-- |Calculate the attraction force between two connected particles.
attract :: Particle -> Particle -> Force
attract this other = hookes springConstant (pos other) (pos this)
