module ForceGraph.Ball
  ( Ball(..)
  , position
  , integrate
  , limitRect
  ) where

import ForceGraph.Rectangle
import ForceGraph.Types
import qualified ForceGraph.Particle as P

data Ball = Ball
  { particle :: P.Particle
  , radius :: Radius
  , mass :: Mass
  , charge :: Charge
  } deriving Show

mapParticle :: (P.Particle -> P.Particle) -> Ball -> Ball
mapParticle f b = b { particle = f (particle b) }

position :: Ball -> Point
position = P.x1 . particle

integrate :: Double -> Ball -> Ball
integrate = mapParticle . P.integrate

limitRect :: Rectangle -> Ball -> Ball
limitRect = mapParticle . P.limitRect
