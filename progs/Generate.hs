{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Grapher.Generation
import System.Environment
import System.IO
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = getArgs >>= \case
  ["bin-tree", n, h] -> go (binaryTree (read n) (read h))
  ["grid", w, h] -> go (grid (read w) (read h))
  ["circle", n] -> go (circle (read n))

  _ -> do
    exe <- getProgName
    hPutStr stderr $
      "Usage: " ++ exe ++ " GRAPH OPTIONS...\n\
      \  Available graphs:\n\
      \    bin-tree node-count height\n\
      \    grid width height\n\
      \    circle node-count\n"

  where
    go = T.putStrLn . toLazyText . builder

builder :: Graph -> Builder
builder graph = "digraph {" <> mmconcat (map edge (snd graph)) <> "}"
  where
    edge (i, j) = decimal i <> "--" <> decimal j <> ";"
