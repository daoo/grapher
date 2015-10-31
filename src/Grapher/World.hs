{-# LANGUAGE BangPatterns #-}
module Grapher.World
  ( Graph
  , World(worldNodes, worldEdges)
  , newWorld
  , particle
  , hasEdge

  , modify

  , iteration
  ) where

import Data.Function
import Data.Vector.Unboxed (Vector)
import Grapher.AdjacencyMatrix
import Grapher.Particle
import Grapher.Physics
import Grapher.Vector2F
import Prelude hiding (pi)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V (unsafeRead, unsafeWrite)

repelConstant, springConstant, airDragConstant :: Float
repelConstant   = -300
springConstant  = 100
airDragConstant = 200

particleMass, particleCharge :: Float
particleMass   = 1.0
particleCharge = 10.0

type Graph = (Int, [(Int, Int)])

data World = World
  { worldNodes :: !(Vector Particle)
  , worldEdges :: !Matrix
  } deriving Show

{-# INLINE particle #-}
particle :: World -> Int -> Particle
particle w !i = worldNodes w `V.unsafeIndex` i

modify :: (Particle -> Particle) -> Int -> World -> World
modify f !i w = w { worldNodes = V.modify helper (worldNodes w) }
  where
    helper v = V.unsafeRead v i >>= \p -> V.unsafeWrite v i (f p)

{-# INLINE hasEdge #-}
hasEdge :: World -> Int -> Int -> Bool
hasEdge = isAdjacent . worldEdges

-- |Create a new world from a number of nodes and a list of edges.
newWorld :: Graph -> World
newWorld (count, edges) = World (V.generate count new) (newMatrix count edges)
  where
    a :: Double
    a = sqrt $ fromIntegral count

    b :: Int
    b = round a

    new :: Int -> Particle
    new i = fromPoint (fromIntegral x :+ fromIntegral y)
      where
        (x, y) = i `quotRem` b

iteration :: Float -> World -> World
iteration delta w = w { worldNodes = V.imap f $ worldNodes w }
  where
    f i b = integrate delta $ force particleMass (forceAll w i b) b

{-# INLINE forceAll #-}
forceAll :: World -> Int -> Particle -> Vector2F
forceAll !w !i !pi =
  forceDrag pi +
  forceCenter pi +
  V.sum (V.imap (forceInteractive w i pi) (worldNodes w))
  where

{-# INLINE forceInteractive #-}
forceInteractive :: World -> Int -> Particle -> Int -> Particle -> Vector2F
forceInteractive !w !i !pi !j !pj
  | i == j        = zero
  | hasEdge w i j = forceAttract pi pj + frepel
  | otherwise     = frepel

  where
    frepel = forceRepel pi pj

{-# INLINE forceDrag #-}
-- |Calculate the air drag force exerted on a particle.
--
-- The force is linearly proportional to the speed of the particle.
forceDrag :: Particle -> Vector2F
forceDrag p = negate (airDragConstant .* vel p)

{-# INLINE forceCenter #-}
-- |Calculates the pulling force towards the center.
--
-- The force is linearly proportional to the distance from the center.
forceCenter :: Particle -> Vector2F
forceCenter = negate . pos

{-# INLINE forceRepel #-}
-- |Calculate the repel force exerted by the first particle on the second.
forceRepel :: Particle -> Particle -> Vector2F
forceRepel this other = interaction repelConstant (f other) (f this)
  where
    f x = (pos x, particleCharge)

{-# INLINE forceAttract #-}
-- |Calculate the attractive force between two connected particles.
forceAttract :: Particle -> Particle -> Vector2F
forceAttract this other = hookes springConstant (pos other) (pos this)
