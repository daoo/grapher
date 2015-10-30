{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Function
import Data.Maybe
import Data.Text.IO as T
import Grapher.AdjacencyMatrix
-- import Grapher.Generation
import Grapher.Parser
import Grapher.Particle
import Grapher.Vector2F
import Grapher.World hiding (nodes, edges)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import System.Environment
import qualified Data.Vector.Unboxed as V
import qualified Grapher.World as W

vtup :: Vector2F -> (Float, Float)
vtup (x:+y) = (x, y)

tupv :: (Float, Float) -> Vector2F
tupv = uncurry (:+)

--worldInit :: World
--worldInit = uncurry newWorld (binaryTree 1 50)
--worldInit = uncurry newWorld (binaryTree 2 9)
--worldInit = uncurry newWorld (grid 10 10)
--worldInit = uncurry newWorld (Grapher.Generation.circle 50)

data UI = UI !(Maybe Int) !Vector2F !ViewState !World

uiInit :: World -> UI
uiInit w = UI Nothing (0:+0) viewStateInit w

main :: IO ()
main = do
  [path] <- getArgs
  txt <- T.readFile path
  world <- case parseGraph txt of
    Left err -> error err
    Right graph -> return (uncurry newWorld (discardNodeData graph))

  play (InWindow "Force Graph" (800, 600) (0, 0))
    white 100 (uiInit world) render input update

input :: Event -> UI -> UI
input event (UI active mouse view world) = case event of

  EventKey (MouseButton LeftButton) Down _ mouse'
    | isJust ix -> UI ix point view world
    where
      point = tupv (invertViewPort (viewStateViewPort view) mouse')
      ix = V.findIndex (isSelected point) (W.nodes world)

  EventKey (MouseButton LeftButton) Up _ _
    | isJust active -> UI Nothing mouse view world

  EventMotion mouse'
    | isJust active ->
      UI active (tupv (invertViewPort (viewStateViewPort view) mouse')) view world

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
    nodes = map rendern (V.toList $ W.nodes world)
    edges = withAdjacent rendere (W.edges world)

    renderh = renderHighlight . pos . particle world
    rendern = renderNode . pos
    rendere = renderEdge `on` (pos . particle world)

renderEdge :: Vector2F -> Vector2F -> Picture
renderEdge pa pb = line [vtup pa, vtup pb]

renderNode :: Vector2F -> Picture
renderNode p = uncurry translate (vtup p) $
  circleSolid nodeRadius

renderHighlight :: Vector2F -> Picture
renderHighlight p = color green $ uncurry translate (vtup p) $
  circleSolid highlightRadius

isSelected :: Vector2F -> Particle -> Bool
isSelected point part = (point `dist2` pos part) < highlightRadiusSquared

nodeRadius, highlightRadius, highlightRadiusSquared :: Float
nodeRadius = 10
highlightRadius = 20
highlightRadiusSquared = highlightRadius * highlightRadius
