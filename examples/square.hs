{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, QuasiQuotes #-}
module Main where

import JavaScript.Canvas
import JavaScript.JQuery
import GHCJS.Types
import GHCJS.Foreign
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import GHCJS.Foreign.QQ

newtype Scale = Scale { unScale :: (Int, Int) } deriving (Eq, Show)

scaleWidthHeight :: Scale -> (Int,Int) -> (Int, Int)
scaleWidthHeight (Scale (absWidth, absHeight)) (width, height) = (floor width' , floor height')
  where width' = (fromIntegral $ width * absWidth) / 1000.0 :: Double
        height' = (fromIntegral $ height * absHeight )/ 1000.0 :: Double
main = do
  let sc = Scale (200,200)
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  i <- createImage "test.png"
  g <- createImage "animation-tankBlue.png"
  let fullPercentage = 0.5
  (width, height) <- imageDimensions g
  drawImage i 0 0 124 200 ctx
  drawImageSlice g 0 (height - (getPictureSlice 100 1000 fullPercentage)) width (getPictureSlice 100 1000 fullPercentage) 0 (200 - 200 * fullPercentage) 124 (200 * fullPercentage) ctx
  drawLines ctx
  translate 0 (185 - 180 * fullPercentage) ctx
  drawCenterLine ctx
  translate 155 0 ctx
  drawTriangle ctx
  return ()

getPictureSlice :: Double -> Double -> Double -> Double
getPictureSlice minHeight maxHeight percentage = minHeight + ((maxHeight - minHeight) * percentage)

drawTriangle :: Context -> IO ()
drawTriangle ctx = do
  beginPath ctx
  moveTo 0 0 ctx
  lineTo 7.6 (-7.8) ctx
  lineTo 7.6 (7.8) ctx
  fill ctx


drawCenterLine :: Context -> IO ()
drawCenterLine ctx = do
  beginPath ctx
  lineWidth 0.5 ctx
  moveTo 10 0 ctx
  lineTo 155 0 ctx
  stroke ctx
  closePath ctx


drawLines :: Context -> IO ()
drawLines ctx = do
  beginPath ctx 
  moveTo 155 10 ctx
  lineTo 155 190 ctx
  lineWidth 2 ctx
  stroke ctx
  lineWidth 1 ctx
  translate 150 10 ctx
  mapM_ makeTick [0,18..180]
  translate (-150) (-5) ctx
  where makeTick :: Double -> IO ()
        makeTick y = do
          beginPath ctx
          moveTo 0 (y) ctx
          lineTo 5 (y) ctx
          stroke ctx
          closePath ctx


-- | Look at https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Using_images#Slicing for more
-- info
drawImageSlice :: Image -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
drawImageSlice img sx sy sWidth sHeight dx dy dWidth dHeight ctx = do
  [js_|`ctx.drawImage(`img, `sx, `sy, `sWidth, `sHeight, `dx, `dy, `dWidth, `dHeight)|]



imageDimensions :: Image -> IO (Double, Double)
imageDimensions img = (,) <$> (imageWidth img) <*> (imageHeight img)

imageHeight :: Image -> IO Double
imageHeight img = [js|`img.height|]

imageWidth :: Image -> IO Double
imageWidth img = [js|`img.width|]

createImage :: String -> IO Image
createImage imageName = do
  image <- newImage
  [js_| `image.src = `imageName|]
  mv <- newEmptyMVar
  cb <- syncCallback AlwaysRetain True (putMVar mv ())
  [js_|`image.onload = `cb|]
  _ <- takeMVar mv
  return image

newImage :: IO Image
newImage = [js| new Image() |]

-- displayGauge :: Context -> Double -> IO ()
-- displayGauge ctx height = do
--   translate 20 20 ctx
--   drawInside ctx 130
--   drawOutline ctx
--   threadDelay 1000000 -- do
  -- let scaleWH = scaleWidthHeight sc
  --     scaleW = \w -> fst $ scaleWidthHeight sc (w,0)
  --     scaleH = \h -> snd $ scaleWidthHeight sc (0,h)
  -- save ctx
  -- fillStyle 255 255 255 1 ctx
  -- restore ctx
  -- save ctx
  -- -- fillRect (-50) (20) 50 100 ctx
  -- scale 2 1 ctx
  -- beginPath ctx
  -- arc 0 65 30 0 ( 2 * pi) False ctx
  -- restore ctx
  -- -- fillStyle 142 214 255 1 ctx
  -- -- fill ctx
  -- lineWidth 2 ctx
  -- stroke ctx
  -- threadDelay 100000


-- drawFill :: Double -> Context -> IO ()
-- drawFill height ctx = do
--   globalAlpha 0.9 ctx
--   drawInside ctx height

-- drawInside :: Context -> Double -> IO ()
-- drawInside ctx height = do
--   beginPath ctx
--   translate 0 (161 - height) ctx
--   fillStyle 0 0 0 1 ctx
--   fillRect 0 0 100 height ctx
--   closePath ctx
--   translate 0 (- (161 - height)) ctx

-- drawOutline :: Context -> IO ()
-- drawOutline ctx = do
--   beginPath ctx
--   fillStyle 255 255 255 0.6 ctx
--   lineWidth 2 ctx
--   strokeRect 0 0 100 161 ctx
--   closePath ctx
--   beginPath ctx
--   drawTicks ctx (100,161) (-5)
--   translate (-100) 0 ctx
--   drawTicks ctx (100,161) (5)
--   beginPath ctx






-- drawTicks :: Context -> (Double, Double) -> Double -> IO ()
-- drawTicks ctx (x,y) tickLength = mapM_ (ticks tickLength) [(x,y/4),(x,y/2),(x,(3 * y / 4))]
--   where ticks :: Double -> (Double, Double) -> IO ()
--         ticks tickLength (x,y) = do
--           beginPath ctx 
--           moveTo x y ctx
--           lineTo (x + tickLength) y ctx
--           lineWidth 2 ctx
--           stroke ctx


-- drawLine :: Context -> Double -> (Double, Double) -> IO ()
-- drawLine ctx tickLength (x,y) = do
--   beginPath ctx
--   moveTo 0 0 ctx
--   lineTo x y ctx
--   lineWidth 2 ctx
--   stroke ctx
--   mapM_ (leftTick tickLength) [(x,y/4),(x,y/2),(x,(3 * y / 4))]
--   where leftTick :: Double -> (Double, Double) -> IO ()
--         leftTick tickLength (x,y) = do
--           beginPath ctx 
--           moveTo x y ctx
--           lineTo (x + tickLength) y ctx
--           lineWidth 1 ctx
--           stroke ctx


-- drawBottom :: Context -> IO ()
-- drawBottom ctx = do
--   beginPath ctx
--   save ctx
--   fillStyle 255 255 255 1 ctx
--   restore ctx
--   save ctx
--   scale 2 1 ctx
--   beginPath ctx
--   arc 0 0 30 0 ( 2 * pi) False ctx
--   restore ctx
--   lineWidth 2 ctx
--   stroke ctx

-- data TankGauge = TankGauge {
--   tankGaugeMaximum :: Int
-- , tankGaugeValue :: Int
-- } deriving (Eq, Show)

