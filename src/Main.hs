{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow

import Backend.Backend (defaultSettings)
import Backend.Cairo ()
import qualified Graphics.UI.Gtk as Gtk

import Reactive.Banana

import Defaults
import World

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

draw :: Gtk.DrawingArea -> World -> IO ()
draw canvas world = do
  dw <- Gtk.widgetGetDrawWindow canvas
  size@(w, h) <- Gtk.widgetGetSize canvas

  regio <- Gtk.regionRectangle $ Gtk.Rectangle 0 0 w h
  Gtk.drawWindowBeginPaintRegion dw regio
  Gtk.renderWithDrawable dw $
    render defaultSettings (f size) (worldObjects world) (worldConnections world)
  Gtk.drawWindowEndPaint dw

  where f = realToFrac *** realToFrac

setupNetwork :: World -> (World -> IO ()) -> EventSource () -> IO EventNetwork
setupNetwork world d esLoop = compile $ do
  eLoop <- fromAddHandler (addHandler esLoop)
  let eWorld = accumE world $ (iteration 10 <$ eLoop)
  reactimate $ d <$> eWorld

main :: IO ()
main = do
  _ <- Gtk.initGUI

  w <- Gtk.windowNew
  c <- Gtk.drawingAreaNew

  Gtk.set w [ Gtk.windowTitle Gtk.:= "Graph"
            , Gtk.containerChild Gtk.:= c ]

  esLoop <- newAddHandler
  network <- setupNetwork defaultWorld (draw c) esLoop
  actuate network

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
  _ <- w `Gtk.on` Gtk.configureEvent $ liftIO (fire esLoop ()) >> return False

  _ <- Gtk.timeoutAdd (liftIO (fire esLoop ()) >> return True) 100

  Gtk.widgetShowAll w
  Gtk.mainGUI
