{-# LANGUAGE BangPatterns, LambdaCase #-}
module Main (main) where

import Data.Function
import Data.Maybe
import Grapher.AdjacencyMatrix
import Grapher.Parser
import Grapher.Particle
import Grapher.World
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import Linear.Metric
import Linear.V2
import Linear.Vector
import System.Environment
import qualified Data.Vector.Unboxed as V

data UI = UI !(Maybe Int) !(V2 Float) !ViewState !World

uiInit :: World -> UI
uiInit w = UI Nothing zero viewStateInit w

main :: IO ()
main = getArgs >>= \case
  [path] -> readFile path >>= either error prog . parseDot
  _ -> getContents >>= either error prog . parseDot

prog :: Graph -> IO ()
prog graph = play window white 100 world render input update
  where
    window = InWindow "Force Graph" (800, 600) (0, 0)
    world = uiInit (newWorld graph)

input :: Event -> UI -> UI
input event (UI active mouse view world) = case event of

  EventKey (MouseButton LeftButton) Down _ mouse'
    | isJust ix -> UI ix point view world
    where
      point = uncurry V2 (invertViewPort (viewStateViewPort view) mouse')
      ix = V.findIndex (isSelected point) (worldNodes world)

  EventKey (MouseButton LeftButton) Up _ _
    | isJust active -> UI Nothing mouse view world

  EventMotion mouse'
    | isJust active ->
      UI active (uncurry V2 (invertViewPort (viewStateViewPort view) mouse')) view world

  _ -> UI active mouse (updateViewStateWithEvent event view) world

updateDragging :: UI -> UI
updateDragging ui@(UI Nothing _ _ _) = ui
updateDragging (UI active@(Just ix) mouse view world) =
  UI active mouse view (modify (const (fromPoint mouse)) ix world)

update :: Float -> UI -> UI
update time (UI active mouse view world) =
  updateDragging $ UI active mouse view (iteration time world)

render :: UI -> Picture
render (UI active _ view world) = applyViewPortToPicture (viewStateViewPort view) picture
  where
    picture = mconcat (highlight : nodes ++ edges)

    highlight = maybe mempty renderh active
    nodes = map rendern (V.toList $ worldNodes world)
    edges = withAdjacent rendere (worldEdges world)

    renderh = renderHighlight . pos . particle world
    rendern = renderNode . pos
    rendere = renderEdge `on` (pos . particle world)

renderEdge :: V2 Float -> V2 Float -> Picture
renderEdge (V2 ax ay) (V2 bx by) = line [(ax,ay), (bx,by)]

renderNode :: V2 Float -> Picture
renderNode (V2 x y) = uncurry translate (x,y) $
  circleSolid nodeRadius

renderHighlight :: V2 Float -> Picture
renderHighlight (V2 x y) = color green $ uncurry translate (x,y) $
  circleSolid highlightRadius

isSelected :: V2 Float -> Particle -> Bool
isSelected point part = (point `qd` pos part) < highlightRadiusSquared

nodeRadius, highlightRadius, highlightRadiusSquared :: Float
nodeRadius = 10
highlightRadius = 20
highlightRadiusSquared = highlightRadius * highlightRadius
