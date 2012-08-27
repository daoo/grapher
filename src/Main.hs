{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow
import ForceGraph.Backend.Cairo
import ForceGraph.Defaults
import ForceGraph.Rectangle
import ForceGraph.Time
import ForceGraph.World
import Reactive.Banana
import qualified Graphics.UI.Gtk as Gtk

setupNetwork :: World -> (World -> IO ()) -> AddHandler Double -> AddHandler Rectangle -> IO EventNetwork
setupNetwork world draw estime esconfigure = compile $ do
  etime <- fromAddHandler estime
  econfigure <- fromAddHandler esconfigure

  let eiteration = iteration <$> etime
      ecircle = updateWorld <$> econfigure
      eworld = accumE world (eiteration `union` ecircle)

  reactimate $ draw <$> eworld

updateWorld :: Rectangle -> World -> World
updateWorld a w = w { worldBoundary = a }

main :: IO ()
main = do
  _ <- Gtk.initGUI

  w <- Gtk.windowNew
  c <- Gtk.drawingAreaNew

  Gtk.set w [ Gtk.windowTitle Gtk.:= "Graph"
            , Gtk.containerChild Gtk.:= c ]

  clock <- newClock

  esloop <- newAddHandler
  esconfigure <- newAddHandler
  network <- setupNetwork defaultWorld (drawWorld c) (fst esloop) (fst esconfigure)
  actuate network

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
  _ <- w `Gtk.on` Gtk.configureEvent $ liftIO (updateSize w (snd esconfigure)) >> return False
  _ <- Gtk.timeoutAdd (timeout clock (snd esloop)) 10

  Gtk.widgetShowAll w
  Gtk.mainGUI

  where
    timeout clock event = do
      delta <- clockDelta clock
      _ <- event delta
      return True

updateSize :: Gtk.Window -> (Rectangle -> IO ()) -> IO ()
updateSize window f = do
  (w, h) <- fmap (realToFrac *** realToFrac) $ Gtk.widgetGetSize window
  f $ Rectangle w h
