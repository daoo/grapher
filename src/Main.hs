{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ForceGraph.Backend.Cairo
import ForceGraph.Defaults
import ForceGraph.Time
import ForceGraph.World
import Math.Vector2
import Reactive.Banana
import qualified Graphics.UI.Gtk as Gtk

setupNetwork :: World -> (World -> IO ()) -> AddHandler Double -> AddHandler (Vector2D, Double) -> IO EventNetwork
setupNetwork world draw esTime esConfigure = compile $ do
  eTime <- fromAddHandler esTime
  eConfigure <- fromChanges (Vector2 0 0, 0) esConfigure
  reactimate $ draw <$> apply (updateWorld <$> eConfigure) (accumE world $ iteration <$> eTime)

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

  esLoop <- newAddHandler
  esConfigure <- newAddHandler
  network <- setupNetwork defaultWorld (drawWorld c) (fst esLoop) (fst esConfigure)
  actuate network

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
  _ <- w `Gtk.on` Gtk.configureEvent $ liftIO (updateSize c (snd esConfigure)) >> return False
  _ <- Gtk.timeoutAdd (timeout clock (snd esLoop)) 10

  Gtk.widgetShowAll w
  Gtk.mainGUI

  where
    timeout clock event = do
      delta <- clockDelta clock
      _ <- event delta
      return True

updateSize :: Gtk.DrawingArea -> ((Vector2D, Double) -> IO ()) -> IO ()
updateSize c f = do
  (w, h) <- fmap (\(Gtk.Requisition w h) -> (realToFrac w, realToFrac h)) $ Gtk.widgetSizeRequest c
  f (p w h, r w h)
  where
    p w h = Vector2 (w / 2) (h / 2)
    r w h = case w `compare` h of
      GT -> h * 9 / 10.0
      _  -> w * 9 / 10.0
