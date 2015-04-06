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
import Grapher.World
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game
import System.Environment
import qualified Data.Vector.Unboxed as V

vtup :: Vector2F -> (Float, Float)
vtup (x:+y) = (x, y)

tupv :: (Float, Float) -> Vector2F
tupv = uncurry (:+)

--worldInit :: World
--worldInit = uncurry newWorld (binaryTree 1 50)
--worldInit = uncurry newWorld (binaryTree 2 9)
--worldInit = uncurry newWorld (grid 10 10)
--worldInit = uncurry newWorld (Grapher.Generation.circle 50)

data UI = UI
  { activeNode :: !(Maybe Int)
  , mousePos   :: !Vector2F
  , world      :: !World
  , viewState  :: !ViewState
  }

nodesUI :: UI -> V.Vector Particle
nodesUI = nodes . world

invertUI :: UI -> Point -> Vector2F
invertUI ui p = tupv (invertViewPort (viewStateViewPort (viewState ui)) p)

updateViewStateWithEventUI :: UI -> Event -> UI
updateViewStateWithEventUI ui event = ui
  { viewState = updateViewStateWithEvent event (viewState ui) }

main :: IO ()
main = do
  [path] <- getArgs
  txt <- T.readFile path
  case parseGraph txt of
    Left err -> error err
    Right apa ->
      play
        (InWindow "Force Graph" (800, 600) (0, 0))
        white
        100
        (UI Nothing (0:+0) (uncurry newWorld (toGraph apa)) viewStateInit)
        render
        input
        update

input :: Event -> UI -> UI
input event ui = case event of

  EventKey (MouseButton LeftButton) Down _ p
    | isJust i -> ui { activeNode = i, mousePos = p' }
    where
      p' = invertUI ui p
      i = V.findIndex (f p') (nodesUI ui)

  EventKey (MouseButton LeftButton) Up _ _ | isJust (activeNode ui) -> ui { activeNode = Nothing }

  EventMotion p | isJust (activeNode ui) -> ui { mousePos = invertUI ui p }

  _ -> updateViewStateWithEventUI ui event

  where
    f x p = (x `dist2` pos p) < nodeRadiusSquared

updateDragging :: UI -> UI
updateDragging ui = maybe
  ui
  (\i -> ui { world = modify (const (fromPoint (mousePos ui))) i (world ui) })
  (activeNode ui)

update :: Float -> UI -> UI
update t ui = updateDragging $ ui { world = iteration t (world ui) }

render :: UI -> Picture
render ui = applyViewPortToPicture (viewStateViewPort $ viewState ui) $ mconcat $
  [maybe mempty (renderHighlight . pos . particle (world ui)) (activeNode ui)] ++
  map (renderNode . pos) (V.toList $ nodes (world ui)) ++
  withAdjacent (renderEdge `on` (pos . particle (world ui))) (edges (world ui))

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
