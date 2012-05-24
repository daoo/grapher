module Main where

import Data.Graph

type Point    = (Double, Double)
type Velocity = (Double, Double)
type Mass     = Double
type Time     = Double

data Object = Object
  { vert :: Vertex
  , pos :: Point
  , vel :: Velocity
  , mass :: Mass }

instance Eq Object where
  o1 == o2 = vert o1 == vert o2
  o1 /= o2 = vert o1 /= vert o2

origo :: Point
origo = (0, 0)

zero :: Velocity
zero = (0, 0)

maxForce :: Double
maxForce = 100

gravity :: Double
gravity = 10

distanceSquared :: Point -> Point -> Double
distanceSquared (x1, y1) (x2, y2) = (x2 - x1) ^ 2 + (y2 - x1) ^ 2

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ distanceSquared p1 p2

force :: Object -> Object -> Double
force o1 o2 = (gravity * mass o1 * mass o2) / (distanceSquared (pos o1) (pos o2))

objs :: [Object]
objs = [ Object 0 origo zero 1
       , Object 1 origo zero 1
       , Object 2 origo zero 1
       , Object 3 origo zero 1 ]

defaultGraph :: Graph
defaultGraph = buildG (0, 3)
  [ (0, 1)
  , (1, 2)
  , (2, 3)
  , (3, 0)
  ]

iteration :: Graph -> [Object] -> Time -> [Object]
iteration = undefined

push :: Time -> Object -> [Object] -> [Object]
push t x = map f
  where
    f y = let force = min (maxForce, force x y)

main = do
  print defaultGraph
