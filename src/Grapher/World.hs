module Grapher.World
  ( World()
  , newWorld
  , particleList
  , linkList
  , particlesWithLinks

  , iteration
  ) where

import Data.Function
import Data.Monoid
import Data.Vector (Vector)
import Grapher.AdjacencyMatrix
import Grapher.Particle
import Grapher.Physics
import Grapher.Types
import Grapher.Vector2F
import Numeric.FastMath ()
import qualified Data.Vector as V

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

{-# INLINE particleList #-}
particleList :: World -> [Vector2F]
particleList = map pos . V.toList . vector

{-# INLINE linkList #-}
linkList :: (Vector2F -> Vector2F -> a) -> World -> [a]
linkList f w = withAdjacent (f `on` (pos . particle w)) (matrix w)

{-# INLINE particlesWithLinks #-}
particlesWithLinks :: Monoid a => (Vector2F -> a) -> (Vector2F -> Vector2F -> a) -> World -> [a]
particlesWithLinks f g (World v m) = V.toList $ V.imap h v
  where
    h i a = f (pos a) <> mconcat (adjacentTo (g (pos a) . pos . V.unsafeIndex v) m i)

newWorld :: [Particle] -> [Link] -> World
newWorld parts links = World v (newMatrix (V.length v) links)
  where
    v = V.fromList parts

iteration :: Float -> World -> World
iteration delta w = w { vector = V.imap f $ vector w }
  where
    f i b = integrate delta $ force (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces w i bi = airDrag bi + centerPull bi + V.sum (V.imap intrct (vector w))
  where
    intrct j bj
      | i == j       = zero
      | linked w i j = attract bi bj + frepell
      | otherwise    = frepell

      where
        frepell = repell bi bj

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
