{-# LANGUAGE BangPatterns #-}
module Grapher.World
  ( World()
  , newWorld
  , particleList
  , linkList
  , particlesWithLinks

  , iteration
  ) where

import Prelude hiding (pi)
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

repelConstant, springConstant, airDragConstant :: Float
repelConstant   = -100
springConstant  = 100
airDragConstant = 200

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
forces !w !i !pi =
  forceDrag pi +
  forceCenter pi +
  V.sum (V.imap (forceInteractive w i pi) (vector w))
  where

forceInteractive :: World -> Int -> Particle -> Int -> Particle -> Force
forceInteractive !w !i !pi !j !pj
  | i == j       = zero
  | linked w i j = forceAttract pi pj + frepel
  | otherwise    = frepel

  where
    frepel = forceRepel pi pj

-- |Calculate the air drag force exerted on a particle.
--
-- The force is linearly proportional to the speed of the particle.
forceDrag :: Particle -> Force
forceDrag p = negate (airDragConstant .* vel p)

-- |Calculates the pulling force towards the center.
--
-- The force is linearly proportional to the distance from the center.
forceCenter :: Particle -> Force
forceCenter = negate . pos

-- |Calcualte the repel force exerted by the first particle on the second.
forceRepel :: Particle -> Particle -> Force
forceRepel this other = interaction repelConstant (f other) (f this)
  where
    f x = (pos x, charge x)

-- |Calculate the attractive force between two connected particles.
forceAttract :: Particle -> Particle -> Force
forceAttract this other = hookes springConstant (pos other) (pos this)
