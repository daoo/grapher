{-# LANGUAGE BangPatterns #-}
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
import Data.Vector.Unboxed (Vector)
import Grapher.AdjacencyMatrix
import Grapher.Particle
import Grapher.Physics
import Grapher.Types
import Grapher.Util
import Grapher.Vector2F
import Numeric.FastMath ()
import Prelude hiding (pi)
import qualified Data.Vector.Unboxed as V

repelConstant, springConstant, airDragConstant :: Float
repelConstant   = -100
springConstant  = 100
airDragConstant = 200

particleMass, particleCharge :: Float
particleMass   = 1.0
particleCharge = 10.0

data World = World
  { worldNodes :: !(Vector Particle)
  , worldEdges :: !Matrix
  } deriving Show

particle :: World -> Int -> Particle
particle w i = worldNodes w `V.unsafeIndex` i

linked :: World -> Int -> Int -> Bool
linked = isAdjacent . worldEdges

{-# INLINE particleList #-}
particleList :: World -> [Vector2F]
particleList = map pos . V.toList . worldNodes

{-# INLINE linkList #-}
linkList :: (Vector2F -> Vector2F -> a) -> World -> [a]
linkList f w = withAdjacent (f `on` (pos . particle w)) (worldEdges w)

{-# INLINE particlesWithLinks #-}
particlesWithLinks :: Monoid a => (Vector2F -> a) -> (Vector2F -> Vector2F -> a) -> World -> [a]
particlesWithLinks f g (World v m) = imap h $ V.toList v
  where
    h i a = f (pos a) <> mconcat (adjacentTo (g (pos a) . pos . V.unsafeIndex v) m i)

newWorld :: [Particle] -> [(Int, Int)] -> World
newWorld parts edges = World v (newMatrix (V.length v) edges)
  where
    v = V.fromList parts

iteration :: Float -> World -> World
iteration delta w = w { worldNodes = V.imap f $ worldNodes w }
  where
    f i b = integrate delta $ force particleMass (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces !w !i !pi =
  forceDrag pi +
  forceCenter pi +
  V.sum (V.imap (forceInteractive w i pi) (worldNodes w))
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
    f x = (pos x, particleCharge)

-- |Calculate the attractive force between two connected particles.
forceAttract :: Particle -> Particle -> Force
forceAttract this other = hookes springConstant (pos other) (pos this)
