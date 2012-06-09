module World where

import Control.Arrow
import Data.List

import Backend.Backend

import Math.Vector2

import Object
import Physics

type Time = Double

type Pair = (Object, Object)
type Connection = (Int, Int)

data World = World
  { worldObjects :: [Object]
  , worldConnections :: [Connection] }
  deriving Show

showWorld :: World -> String
showWorld = intercalate "\n" . map show . worldObjects

iteration :: Time -> World -> World
iteration t (World objs cons) = World (map (i . r) objs) cons
  where
    i :: Object -> Object
    i = integrate t

    r :: Object -> Object
    r obj = foldl repel obj objs

integrate :: Time -> Object -> Object
integrate t obj = obj { pos = pos obj + (t `mult` vel obj) }

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
