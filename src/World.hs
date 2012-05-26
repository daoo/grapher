module World where

import Data.List
import Data.Graph
import Extensions
import Vector2

type Pair     = (Object, Object)
type Point    = Vector2D
type Velocity = Vector2D
type Mass     = Double
type Time     = Double

data Object = Object
  { vert :: Vertex
  , pos :: Point
  , vel :: Velocity
  , mass :: Mass }
  deriving (Show)

instance Eq Object where
  o1 == o2 = vert o1 == vert o2
  o1 /= o2 = vert o1 /= vert o2

maxForce, maxDist, gravity :: Double
maxForce = 100.0
maxDist = 10.0
gravity = 10.0

force :: Object -> Object -> Double
force o1 o2 = (gravity * mass o1 * mass o2) / (distSquared (pos o1) (pos o2))

vertex :: Int -> [Object] -> Maybe Object
vertex i = find ((== i) . vert)

iteration :: Time -> Graph -> [Object] -> [Object]
iteration t g = integrate t . magic repel

integrate :: Time -> [Object] -> [Object]
integrate t = map (\o -> o { pos = (pos o + (t `mult` vel o))})

-- |Constraint movement between two objects
constraint :: Object -> Object -> Object
constraint a b = let da = pos b - pos a
                     db = pos a - pos b
                  in a

-- |Calculate and apply force between two objects
repel :: Pair -> Pair
repel (a, b) =
  let val  = min (force a b) maxForce
      da   = normalize $ (pos b) - (pos a)
      db   = normalize $ (pos a) - (pos b)
      mtot = mass a + mass b
      ra   = mass a / mtot
      rb   = mass b / mtot
      va   = (ra * val) `mult` da
      vb   = (rb * val) `mult` db
   in (a {vel = vel a + va}, b {vel = vel b + vb})
