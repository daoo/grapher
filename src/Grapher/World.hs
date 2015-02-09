{-# LANGUAGE BangPatterns #-}
module Grapher.World
  ( World(nodes, edges)
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
import Grapher.Types
import Grapher.Vector2F
import Prelude hiding (pi)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V (read, write)

repelConstant, springConstant, airDragConstant :: Float
repelConstant   = -100
springConstant  = 100
airDragConstant = 200

particleMass, particleCharge :: Float
particleMass   = 1.0
particleCharge = 10.0

data World = World
  { nodes :: !(Vector Particle)
  , edges :: !Matrix
  } deriving Show

particle :: World -> Int -> Particle
particle w i = nodes w `V.unsafeIndex` i

modify :: (Particle -> Particle) -> Int -> World -> World
modify f i w = w { nodes = V.modify helper (nodes w) }
  where
    helper v = V.read v i >>= \p -> V.write v i (f p)

hasEdge :: World -> Int -> Int -> Bool
hasEdge = isAdjacent . edges

-- |Create a new world from a number of nodes and a list of edges.
newWorld :: Int -> [(Int, Int)] -> World
newWorld c es = World (V.generate c new) (newMatrix c es)
  where
    a :: Double
    a = sqrt $ fromIntegral c

    b :: Int
    b = round a

    new :: Int -> Particle
    new i = mkParticle (fromIntegral x :+ fromIntegral y)
      where
        (x, y) = i `quotRem` b

iteration :: Float -> World -> World
iteration delta w = w { nodes = V.imap f $ nodes w }
  where
    f i b = integrate delta $ force particleMass (forces w i b) b

forces :: World -> Int -> Particle -> Force
forces !w !i !pi =
  forceDrag pi +
  V.sum (V.imap (forceInteractive w i pi) (nodes w))
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

-- -- |Calculates the pulling force towards the center.
-- --
-- -- The force is linearly proportional to the distance from the center.
-- forceCenter :: Particle -> Force
-- forceCenter = negate . pos

-- |Calcualte the repel force exerted by the first particle on the second.
forceRepel :: Particle -> Particle -> Force
forceRepel this other = interaction repelConstant (f other) (f this)
  where
    f x = (pos x, particleCharge)

-- |Calculate the attractive force between two connected particles.
forceAttract :: Particle -> Particle -> Force
forceAttract this other = hookes springConstant (pos other) (pos this)
