module Backend.Cairo where

import Math.Vector2
import Backend.Backend
import Graphics.Rendering.Cairo

instance Backend Render where
  setColor (r, g, b) = setSourceRGB r g b
  setLineWidth = Graphics.Rendering.Cairo.setLineWidth

  strokeRectangle (Vector2 x y) (w, h) = rectangle x y w h >> stroke
  fillRectangle (Vector2 x y) (w, h)   = rectangle x y w h >> fill

  strokeLine (Vector2 x1 y1, Vector2 x2 y2) = moveTo x1 y1 >> lineTo x2 y2 >> stroke

  fillCircle r (Vector2 x y)  = arc x y r 0 pi2 >> fill
  fillArcs r (Vector2 x y) cs = mapM_ f cs >> fill
    where
      f (c, (a, b)) = setColor c >> arc x y r a b

