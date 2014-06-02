module Grapher.Particle
  ( Particle
  , mkParticle
  , pos
  , vel
  , charge
  , integrate
  , force
  ) where

import Control.Applicative
import Foreign.Ptr
import Foreign.Storable
import Grapher.Types
import Grapher.Vector2F

data Particle = Particle
  { x1     :: !Point
  , x2     :: !Point
  , accel  :: !Vector2F
  , mass   :: !Mass
  , charge :: !Charge
  } deriving Show

{-# INLINE mkParticle #-}
mkParticle :: Vector2F -> Mass -> Charge -> Particle
mkParticle p = Particle p p zero

{-# INLINE pos #-}
pos :: Particle -> Vector2F
pos = x1

{-# INLINE vel #-}
vel :: Particle -> Vector2F
vel n = x1 n - x2 n

{-# INLINE move #-}
move :: Particle -> Vector2F -> Particle
move n p = n { x1 = p, x2 = x1 n }

{-# INLINE force #-}
force :: Force -> Particle -> Particle
force f p = p { accel = f /. mass p }

{-# INLINE integrate #-}
integrate :: Float -> Particle -> Particle
integrate t p = move p $ pos p + vel p + (t * t) .* accel p

instance Storable Particle where
  {-# INLINE sizeOf #-}
  sizeOf _ =
    sizeOf (undefined :: Point) +
    sizeOf (undefined :: Point) +
    sizeOf (undefined :: Vector2F) +
    sizeOf (undefined :: Mass) +
    sizeOf (undefined :: Charge)

  {-# INLINE alignment #-}
  alignment _ =
    alignment (undefined :: Point) +
    alignment (undefined :: Point) +
    alignment (undefined :: Vector2F) +
    alignment (undefined :: Mass) +
    alignment (undefined :: Charge)

  {-# INLINE peek #-}
  peek ptr = Particle
    <$> peek (castPtr ptr)
    <*> peek (castPtr ptr `plusPtr` s1)
    <*> peek (castPtr ptr `plusPtr` s2)
    <*> peek (castPtr ptr `plusPtr` s3)
    <*> peek (castPtr ptr `plusPtr` s4)

    where
      s1 = sizeOf (undefined :: Point)
      s2 = s1 + sizeOf (undefined :: Point)
      s3 = s2 + sizeOf (undefined :: Vector2F)
      s4 = s3 + sizeOf (undefined :: Mass)

  {-# INLINE poke #-}
  poke ptr p = do
    poke (castPtr ptr) (x1 p)
    poke (castPtr ptr `plusPtr` s1) (x2 p)
    poke (castPtr ptr `plusPtr` s2) (accel p)
    poke (castPtr ptr `plusPtr` s3) (mass p)
    poke (castPtr ptr `plusPtr` s4) (charge p)

    where
      s1 = sizeOf (undefined :: Point)
      s2 = s1 + sizeOf (undefined :: Point)
      s3 = s2 + sizeOf (undefined :: Vector2F)
      s4 = s3 + sizeOf (undefined :: Mass)
