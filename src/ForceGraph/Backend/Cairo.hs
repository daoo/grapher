module ForceGraph.Backend.Cairo where

import Control.Arrow
import ForceGraph.Backend.Backend
import ForceGraph.World
import Math.Vector2
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk

instance Backend Cairo.Render where
  setColor (r, g, b) = Cairo.setSourceRGB r g b
  setLineWidth       = Cairo.setLineWidth

  strokeRectangle (Vector2 x y) (w, h) = Cairo.rectangle x y w h >> Cairo.stroke
  fillRectangle (Vector2 x y) (w, h)   = Cairo.rectangle x y w h >> Cairo.fill

  strokeLine (Vector2 x1 y1, Vector2 x2 y2) = Cairo.moveTo x1 y1 >> Cairo.lineTo x2 y2 >> Cairo.stroke

  fillCircle r (Vector2 x y)   = Cairo.arc x y r 0 pi2 >> Cairo.fill
  strokeCircle r (Vector2 x y) = Cairo.arc x y r 0 pi2 >> Cairo.stroke
  fillArcs r (Vector2 x y) cs  = mapM_ f cs >> Cairo.fill
    where
      f (c, (a, b)) = setColor c >> Cairo.arc x y r a b

drawWorld :: Gtk.DrawingArea -> World -> IO ()
drawWorld canvas world = do
  dw <- Gtk.widgetGetDrawWindow canvas
  size@(w, h) <- Gtk.widgetGetSize canvas

  regio <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 w h
  Gtk.drawWindowBeginPaintRegion dw regio
  Gtk.renderWithDrawable dw $
    render defaultSettings (f size) world
  Gtk.drawWindowEndPaint dw

  where f = realToFrac *** realToFrac
