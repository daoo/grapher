{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.IORef
import ForceGraph.Backend.Cairo
import ForceGraph.Defaults
import ForceGraph.Rectangle
import ForceGraph.Time
import ForceGraph.World
import qualified Graphics.UI.Gtk as Gtk

main :: IO ()
main = do
  _ <- Gtk.initGUI

  w <- Gtk.windowNew
  c <- Gtk.drawingAreaNew

  Gtk.set w [ Gtk.windowTitle Gtk.:= "Graph"
            , Gtk.containerChild Gtk.:= c
            ]

  clock <- newClock
  worldRef <- newIORef defaultWorld

  _ <- w `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False

  _ <- w `Gtk.on` Gtk.configureEvent $ do
    size <- Gtk.eventSize
    liftIO (updateSize worldRef size)
    return False

  _ <- Gtk.timeoutAdd (timeout c worldRef clock) 10

  Gtk.widgetShowAll w
  Gtk.mainGUI

  where
    timeout drawingArea worldRef clock = do
      delta <- clockDelta clock
      modifyIORef worldRef (iteration delta)
      readIORef worldRef >>= drawWorld drawingArea
      return True

updateSize :: IORef World -> (Int, Int) -> IO ()
updateSize worldRef (w, h) =
  modifyIORef worldRef (setBoundary $ Rectangle (realToFrac w) (realToFrac h))
