module ForceGraph.Ball
  ( Ball(..)
  , position
  , velocity
  , integrate
  , setForce
  ) where

import ForceGraph.Types
import Math.Vector2
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

velocity :: Ball -> Velocity
velocity b = P.x2 p - P.x1 p where p = particle b

integrate :: Double -> Ball -> Ball
integrate = mapParticle . P.integrate

setForce :: Force -> Ball -> Ball
setForce f b = mapParticle (\p -> p { P.accel = f ./ mass b }) b
