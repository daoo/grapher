{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
import Data.Function
import Grapher.AdjacencyMatrix
import Grapher.Generation (grid)
import Grapher.Particle
import Grapher.Vector2F
import Grapher.World
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import qualified Data.Vector.Unboxed as V

times :: Int -> (a -> a) -> a -> a
times 0 _ x = x
times i f x = times (i-1) f (f x)

world, world' :: World
world  = newWorld (grid 10 10)
world' = times 10000 (iteration 0.01) world

main :: IO ()
main = writePng "test.png" $
  renderDrawing width height white $
    withTexture (uniformTexture black) $
      withTransformation centering $ do
        mapM_ strokeLine $ withAdjacent (renderEdge `on` (pos . particle world')) (worldEdges world')
        mapM_ fill $ map (renderNode . pos) $ V.toList $ worldNodes world'
  where
    white = PixelRGBA8 255 255 255 255
    black = PixelRGBA8 0 0 0 255

strokeLine :: [Primitive] -> Drawing px ()
strokeLine = stroke w j (c, c)
  where
    w = 1
    j = JoinMiter 0
    c = CapStraight 0

renderNode :: Vector2F -> [Primitive]
renderNode p = circle (toV2 p) nodeRadius

renderEdge :: Vector2F -> Vector2F -> [Primitive]
renderEdge pa pb = line (toV2 pa) (toV2 pb)

toV2 :: Vector2F -> V2 Float
toV2 (x:+y) = V2 x y

width, height :: Int
width  = 2000
height = 2000

halfWidth, halfHeight :: Float
halfWidth  = fromIntegral width / 2.0
halfHeight = fromIntegral height / 2.0

nodeRadius :: Float
nodeRadius = 10

centering :: Transformation
centering = translate (V2 halfWidth halfHeight)
