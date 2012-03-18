module Main where

import AI.CV.OpenCV
import Control.Monad (when)
import Data.Maybe (isNothing)

showFrames :: Int -> IplImage -> Capture -> IO ()
showFrames winNum target cap  = do
  frame <- queryFrame cap
  convertImage frame target 0
  canny target target 30 190 3
  showImage winNum target
  k <- waitKey 5
  when (isNothing k) $ showFrames winNum target cap

main :: IO ()
main = do
  let winNum = 0
  newWindow winNum autoSize
  cap    <- createCameraCapture 0
  frame  <- queryFrame cap
  target <- createImage (getSize frame) 1 iplDepth8u
  showFrames winNum target cap
