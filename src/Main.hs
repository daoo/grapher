{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Backend.Cairo
import qualified Graphics.UI.Gtk as Gtk
import Reactive.Banana

import Defaults
import Time
import World

setupNetwork :: World -> (World -> IO ()) -> AddHandler Double -> IO EventNetwork
setupNetwork world draw handler = compile $ do
  eTime <- fromAddHandler handler
  reactimate $ fmap draw $ accumE world (iteration <$> eTime)

main :: IO ()
main = do
  _ <- Gtk.initGUI

  w <- Gtk.windowNew
  c <- Gtk.drawingAreaNew

  Gtk.set w [ Gtk.windowTitle Gtk.:= "Graph"
            , Gtk.containerChild Gtk.:= c ]

  clock <- newClock

  esLoop <- newAddHandler
  network <- setupNetwork defaultWorld (drawWorld c) (fst esLoop)
  actuate network

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False
  _ <- Gtk.timeoutAdd (timeout clock (snd esLoop)) 10

  Gtk.widgetShowAll w
  Gtk.mainGUI

  where
    timeout clock event = do
      delta <- clockDelta clock
      _ <- event delta
      return True
