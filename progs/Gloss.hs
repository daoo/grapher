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

tupv :: (Float, Float) -> Vector2F
tupv = uncurry (:+)

initialWorld :: World
--initialWorld = uncurry newWorld (binaryTree 1 50)
initialWorld = uncurry newWorld (binaryTree 4 5)
--initialWorld = uncurry newWorld (grid 10 10)
--initialWorld = uncurry newWorld (Grapher.Generation.circle 50)

data UI = UI
  { activeNode :: !(Maybe Int)
  , mousePos   :: !Vector2F
  , world      :: !World
  }

main :: IO ()
main = do
  play
    (InWindow "Force Graph" (800, 600) (0, 0))
    white
    100
    (UI Nothing (0:+0) initialWorld)
    render
    input
    update

input :: Event -> UI -> UI
input event ui = case event of

  EventKey (MouseButton LeftButton) Down _ p -> ui
    { activeNode = V.findIndex (f (tupv p)) (nodes $ world ui)
    , mousePos   = tupv p
    }

  EventKey (MouseButton LeftButton) Up _ _ -> ui
    { activeNode = Nothing }

  EventMotion p -> ui
    { mousePos = tupv p }

  _ -> ui

  where
    f x p = (x `dist2` pos p) < nodeRadiusSquared

updateDragging :: UI -> UI
updateDragging ui = maybe
  ui
  (\i -> ui { world = modify (const (mkParticle (mousePos ui))) i (world ui) })
  (activeNode ui)

update :: Float -> UI -> UI
update t ui = updateDragging $ ui { world = iteration t (world ui) }

render :: UI -> Picture
render ui = mconcat $
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
