{-# LANGUAGE BangPatterns #-}
module Grapher.World
  ( World(worldNodes, worldEdges)
  , newWorld
  , particle
  , hasEdge

  , iteration
  ) where

import Data.Function
import Data.Vector.Unboxed (Vector)
import Grapher.AdjacencyMatrix
import Grapher.Particle
import Grapher.Physics
import Grapher.Types
import Grapher.Vector2F
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
  { worldNodes      :: !(Vector Particle)
  , worldEdges      :: !Matrix
  --, worldNodeForces :: !(Vector Vector2F)
  } deriving Show

particle :: World -> Int -> Particle
particle w i = worldNodes w `V.unsafeIndex` i

hasEdge :: World -> Int -> Int -> Bool
hasEdge = isAdjacent . worldEdges

-- |Create a new world from a number of nodes and a list of edges.
newWorld :: Int -> [(Int, Int)] -> World
newWorld count edges = World (V.generate count new) (newMatrix count edges)
  where
    a :: Double
    a = sqrt $ fromIntegral count

    b :: Int
    b = round a

    new :: Int -> Particle
    new i = mkParticle (fromIntegral x :+ fromIntegral y)
      where
        (x, y) = i `quotRem` b

iteration :: Float -> World -> World
iteration delta w = w { worldNodes = V.imap f $ worldNodes w }
  where
    f i b = integrate delta $ force particleMass (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces !w !i !pi =
  forceDrag pi +
  V.sum (V.imap (forceInteractive w i pi) (worldNodes w))
  where

forceInteractive :: World -> Int -> Particle -> Int -> Particle -> Force
forceInteractive !w !i !pi !j !pj
  | i == j        = zero
  | hasEdge w i j = forceAttract pi pj + frepel
  | otherwise     = frepel

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
