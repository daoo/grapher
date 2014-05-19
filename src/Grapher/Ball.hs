module Grapher.Ball
  ( Ball(..)
  , pos
  , vel
  , force
  , charge
  , integrate
  ) where

import Control.Applicative
import Foreign.Ptr
import Foreign.Storable
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

instance Storable Ball where
  {-# INLINE sizeOf #-}
  sizeOf _ =
    sizeOf (undefined :: P.Particle) +
    sizeOf (undefined :: Radius)

  {-# INLINE alignment #-}
  alignment _ =
    alignment (undefined :: P.Particle) +
    alignment (undefined :: Radius)

  {-# INLINE peek #-}
  peek ptr = Ball
    <$> peek (castPtr ptr)
    <*> peek (castPtr ptr `plusPtr` sizeOf (undefined :: P.Particle))

  {-# INLINE poke #-}
  poke ptr ball = do
    poke (castPtr ptr) (particle ball)
    poke (castPtr ptr `plusPtr` sizeOf (undefined :: P.Particle)) (radius ball)
