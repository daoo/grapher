{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Codec.Picture.Png
import Codec.Picture.Types
import Grapher.Generation
import Grapher.Vector2F
import Grapher.World
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

times :: Int -> (a -> a) -> a -> a
times 0 _ x = x
times i f x = times (i-1) f (f x)

main :: IO ()
main = do
  let world  = randomWorld 523054 100
  let world' = times 10000 (iteration 0.01) world
      img    = renderDrawing width height white $
        withTexture (uniformTexture black) $ do
          mapM_ strokeLine $ linkList link world'
          mapM_ (fill . ball) $ ballList world'
  writePng "test.png" img

white :: PixelRGB8
white = PixelRGB8 255 255 255

black :: PixelRGB8
black = PixelRGB8 0 0 0

strokeLine :: [Primitive] -> Drawing px ()
strokeLine = stroke w j (c, c)
  where
    w = 1
    j = JoinMiter 0
    c = CapStraight 0

ball :: Vector2F -> [Primitive]
ball a = circle (toV2 a) radius

link :: Vector2F -> Vector2F -> [Primitive]
link a b = line (toV2 a) (toV2 b)

toV2 :: Vector2F -> V2 Float
toV2 a = case vtup a of { (x,y) -> applyTransformation centering $ V2 x y }

width, height :: Int
width  = 2000
height = 2000

radius :: Float
radius = 10

centering :: Transformation
centering = translate (V2 (fromIntegral width/2) (fromIntegral height/2))
