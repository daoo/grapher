{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow

import Backend.Backend (defaultSettings)
import Backend.Cairo ()
import qualified Graphics.UI.Gtk as Gtk
import Math.Vector2
import Reactive.Banana

import World

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

draw :: Gtk.DrawingArea -> [Connection] -> [Object] -> IO ()
draw c cons objs = do
  win <- Gtk.widgetGetDrawWindow c
  size <- Gtk.widgetGetSize c
  Gtk.renderWithDrawable win $
    render defaultSettings (f size) objs cons

  where f = realToFrac *** realToFrac

setupNetwork :: ([Object] -> IO ()) -> EventSource () -> IO EventNetwork
setupNetwork d esDraw = compile $ do
  eDraw <- fromAddHandler (addHandler esDraw)

  let eObjects = accumE defaultObjs $ (id <$ eDraw)

  reactimate $ d <$> eObjects

main :: IO ()
main = do
  _ <- Gtk.initGUI

  w <- Gtk.windowNew
  c <- Gtk.drawingAreaNew

  Gtk.set w [ Gtk.windowTitle Gtk.:= "Graph"
            , Gtk.containerChild Gtk.:= c ]

  esDraw <- newAddHandler
  network <- setupNetwork (draw c defaultGraph) esDraw
  actuate network

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
  _ <- w `Gtk.on` Gtk.configureEvent $ liftIO (fire esDraw ()) >> return False

  Gtk.mainGUI
