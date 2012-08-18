module ForceGraph.Backend.Backend where

import Math.Vector2

type Color = (Double, Double, Double)
type Size  = (Double, Double)
type Line  = (Vector2D, Vector2D)

pi2 :: Double
pi2 = pi * 2.0

class (Monad a) => Backend a where
  setColor :: Color -> a ()
  setLineWidth :: Double -> a ()

  strokeLine :: Line -> a ()
  strokeRectangle :: Vector2D -> Size -> a ()
  fillRectangle :: Vector2D -> Size -> a ()
  fillCircle :: Double -> Vector2D -> a ()
  fillArcs :: Double -> Vector2D -> [(Color, (Double, Double))] -> a ()

data Settings = Settings
  { getFgColor :: Color
  , getBgColor :: Color }

defaultSettings :: Settings
defaultSettings = Settings
  { getFgColor = (0.180392, 0.203922, 0.211765)
  , getBgColor = (0.933333, 0.933333, 0.92549) }
