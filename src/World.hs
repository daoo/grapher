module World where

import Control.Arrow
import Data.List

import Backend.Backend
import Extensions
import Math.Vector2

type Time = Double

type Mass     = Double
type Point    = Vector2D
type Velocity = Vector2D

type Pair = (Object, Object)
type Connection = (Int, Int)

data Object = Object
  { pos :: Point
  , vel :: Velocity
  , mass :: Mass }
  deriving Show

data World = World
  { worldObjects :: [Object]
  , worldConnections :: [Connection] }
  deriving Show

showWorld :: World -> String
showWorld = intercalate "\n" . map showObject . worldObjects

showObject :: Object -> String
showObject (Object p v m) = "Object " ++ show m ++ " " ++ showVector2 p ++ " " ++ showVector2 v

maxForce, maxDist, gravity :: Double
maxForce = 100.0
maxDist = 10.0
gravity = 10000.0

iteration :: Time -> World -> World
iteration t = g (integrate t . magic repel)
  where
    g f w = w { worldObjects = f (worldObjects w) }

integrate :: Time -> [Object] -> [Object]
integrate t = map (\o -> o { pos = pos o + (t `mult` vel o) })

-- |Constraint movement between two objects
constraint :: Object -> Object -> Object
constraint a b = let da = pos b - pos a
                     db = pos a - pos b
                  in a

force :: Object -> Object -> Double
force o1 o2 = (gravity * mass o1 * mass o2) / dist (pos o1) (pos o2)

-- |Calculate and apply force between two objects
repel :: Pair -> Pair
repel (a, b) =
  let val  = min (force a b) maxForce
      da   = normalize $ pos b - pos a
      db   = normalize $ pos a - pos b
      mtot = mass a + mass b
      ra   = mass a / mtot
      rb   = mass b / mtot
      va   = (ra * val) `mult` da
      vb   = (rb * val) `mult` db
   in (a {vel = vel a + va}, b {vel = vel b + vb})

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

