module Main where

import Control.Arrow

import Backend.Backend (defaultSettings)
import Backend.Cairo ()
import Graphics.UI.Gtk hiding (Object())
import Math.Vector2
import Reactive.Banana

import World

setupUI :: IO Window
setupUI = do
  w <- windowNew
  c <- drawingAreaNew

  set w [ windowTitle := "Graph"
        , containerChild := c ]

  _ <- w `on` deleteEvent $ liftIO mainQuit >> return False
  _ <- w `on` configureEvent $ liftIO (draw c) >> return False

  return w

draw :: DrawingArea -> IO ()
draw c = do
  win <- widgetGetDrawWindow c
  size <- widgetGetSize c
  renderWithDrawable win $
    render defaultSettings (f size) defaultObjs defaultGraph

  where f = realToFrac *** realToFrac

main :: IO ()
main = initGUI >> setupUI >>= widgetShowAll >> mainGUI

defaultObjs :: [Object]
defaultObjs =
  [ Object (Vector2 250 250) zero 0
  , Object (Vector2 100 200) zero 1
  , Object (Vector2 200 250) zero 1
  , Object (Vector2 100 540) zero 1 ]

defaultGraph :: [Connection]
defaultGraph =
  [ (0, 1)
  , (0, 2)
  , (2, 3)
  ]
