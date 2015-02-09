{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Function
import Data.Monoid
import Grapher.AdjacencyMatrix
import Grapher.Generation
import Grapher.Particle
import Grapher.Vector2F
import Grapher.World
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector.Unboxed as V

world :: World
--world = uncurry newWorld (binaryTree 1 50)
world = uncurry newWorld (binaryTree 4 5)
--world = uncurry newWorld (grid 10 10)
--world = uncurry newWorld (Grapher.Generation.circle 50)

data UI = UI
  { uiNode :: !(Maybe Int)
  , uiWorld :: !World
  }

main :: IO ()
main = do
  play
    (InWindow "Force Graph" (800, 600) (0, 0))
    white
    100
    (UI Nothing world)
    render
    input
    update

input :: Event -> UI -> UI
input event ui@(UI n w) = case event of

  EventKey (MouseButton LeftButton) Down _ p ->
    UI (V.findIndex (f (uncurry (:+) p)) (nodes w)) w

  EventKey (MouseButton LeftButton) Up _ _ ->
    UI Nothing w

  EventMotion p -> case n of

    Just i -> UI n (modify (const (mkParticle (uncurry (:+) p))) i w)

    Nothing -> ui

  _ -> ui

  where
    f x p = (x `dist2` pos p) < nodeRadiusSquared

update :: Float -> UI -> UI
update t (UI n w) = UI n (iteration t w)

render :: UI -> Picture
render (UI n w) = mconcat $
  [maybe mempty (renderHighlight . pos . particle w) n] ++
  map (renderNode . pos) (V.toList $ nodes w) ++
  withAdjacent (renderEdge `on` (pos . particle w)) (edges w)

renderEdge :: Vector2F -> Vector2F -> Picture
renderEdge pa pb = line [vtup pa, vtup pb]

renderNode :: Vector2F -> Picture
renderNode p = uncurry translate (vtup p) $
  circleSolid nodeRadius

renderHighlight :: Vector2F -> Picture
renderHighlight p = color green $ uncurry translate (vtup p) $
  circleSolid highlightRadius

nodeRadius, nodeRadiusSquared, highlightRadius :: Float
nodeRadius        = 10
nodeRadiusSquared = nodeRadius * nodeRadius
highlightRadius   = 20
