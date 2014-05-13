module Grapher.Ball
  ( Ball(..)
  , pos
  , vel
  , force
  , charge
  , integrate
  ) where

import Grapher.Types
import qualified Grapher.Particle as P

data Ball = Ball
  { particle :: !P.Particle
  , radius   :: !Radius
  } deriving Show

{-# INLINE pos #-}
pos :: Ball -> Point
pos = P.pos . particle

{-# INLINE vel #-}
vel :: Ball -> Velocity
vel = P.vel . particle

{-# INLINE charge #-}
charge :: Ball -> Charge
charge = P.charge . particle

force :: Force -> Ball -> Ball
force f b = b { particle = P.force f (particle b) }

integrate :: Float -> Ball -> Ball
integrate t b = b { particle = P.integrate t (particle b) }
