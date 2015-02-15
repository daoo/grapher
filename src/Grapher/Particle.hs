{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Grapher.Particle
  ( Particle
  , mkParticle
  , pos
  , vel
  , integrate
  , force
  ) where

import Data.Vector.Unboxed.Deriving
import Grapher.Vector2F

data Particle = Particle
  { x1     :: !Vector2F
  , x2     :: !Vector2F
  , accel  :: !Vector2F
  } deriving Show

derivingUnbox "Particle"
  [t| Particle -> (Vector2F, Vector2F, Vector2F) |]
  [| \p -> (x1 p, x2 p, accel p) |]
  [| \(a, b, c) -> Particle a b c |]

{-# INLINE mkParticle #-}
mkParticle :: Vector2F -> Particle
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
force :: Float -> Vector2F -> Particle -> Particle
force m f p = p { accel = f /. m }

{-# INLINE integrate #-}
integrate :: Float -> Particle -> Particle
integrate t p = move p $ pos p + vel p + (t * t) .* accel p
