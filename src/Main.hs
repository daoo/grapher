{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow
import ForceGraph.Backend.Cairo
import ForceGraph.Defaults
import ForceGraph.Time
import ForceGraph.World
import Math.Vector2
import Reactive.Banana
import qualified Graphics.UI.Gtk as Gtk

setupNetwork :: World -> (World -> IO ()) -> AddHandler Double -> AddHandler (Vector2D, Double) -> IO EventNetwork
setupNetwork world draw estime esconfigure = compile $ do
  etime <- fromAddHandler estime
  econfigure <- fromAddHandler esconfigure

  let eiteration = iteration <$> etime
      ecircle = updateWorld <$> econfigure
      eworld = accumE world (eiteration `union` ecircle)

  reactimate $ draw <$> eworld

updateWorld :: (Vector2D, Double) -> World -> World
updateWorld (p, r) w = w { worldLimitPos = p, worldLimitRadius = r }

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

updateSize :: Gtk.Window -> ((Vector2D, Double) -> IO ()) -> IO ()
updateSize window f = do
  (w, h) <- fmap (realToFrac *** realToFrac) $ Gtk.widgetGetSize window
  f (p w h, r w h)
  where
    p w h = Vector2 (w / 2) (h / 2)
    r w h = case w `compare` h of
      GT -> h * 9 / 20.0
      _  -> w * 9 / 20.0
