module World where

import Control.Arrow
import Data.List

import Backend.Backend

import Math.Algebra
import Math.Vector2

type Time = Double

type Mass     = Double
type Charge   = Double
type Point    = Vector2D
type Velocity = Vector2D

type Pair = (Object, Object)
type Connection = (Int, Int)

data Object = Object
  { pos :: Point
  , vel :: Velocity
  , invMass :: Mass
  , charge :: Charge }
  deriving (Show, Eq)

data World = World
  { worldObjects :: [Object]
  , worldConnections :: [Connection] }
  deriving Show

showWorld :: World -> String
showWorld = intercalate "\n" . map show . worldObjects

maxDist, chargeConstant :: Double
maxDist = 100.0
chargeConstant = 100.0

iteration :: Time -> World -> World
iteration t (World objs cons) = World (map (i . r) objs) cons
  where
    i :: Object -> Object
    i = integrate t

    r :: Object -> Object
    r obj = foldl repel obj objs

integrate :: Time -> Object -> Object
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

-- |Constraint movement between two objects
constraint :: Object -> Object -> Object
constraint a b | mag d > maxDist = a { vel = v }
               | otherwise       = a
  where
    d = pos b - pos a
    p = normalize $ orthogonal d
    s = mag (vel a)
    v = s `mult` p

force :: Object -> Object -> Double
force a b = (chargeConstant * charge a * charge b) `divZero` distSquared (pos a) (pos b)

-- |Calculate and apply the force one object affects another with
repel :: Object -> Object -> Object
repel a b = a { vel = vel a + v }
  where
    f = force a b
    d = normalize $ pos a - pos b
    -- mass b / (mass a + mass b) == invMass a / (invMass a + invMass b)
    r = invMass a `divZero` (invMass a + invMass b)
    v = (r * f) `mult` d

connect :: [Object] -> [Connection] -> [Pair]
connect obj = map ((obj !!) *** (obj !!))

render :: Backend a => Settings -> Size -> [Object] -> [Connection] -> a ()
render set (w, h) objs cons = do
  setColor $ getBgColor set
  fillRectangle zero (w, h)

  mapM_ g (connect objs cons)
  mapM_ f objs

  where
    f o = do
      setColor $ getFgColor set
      fillCircle 10 (pos o)
      setColor (0.937255, 0.160784, 0.160784)
      fillCircle 8 (pos o)

    g (o, c) = do
      setColor $ getFgColor set
      strokeLine (pos o, pos c)

