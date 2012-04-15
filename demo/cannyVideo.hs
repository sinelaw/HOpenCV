module Main where

import AI.CV.OpenCV
import Control.Monad (when)
import Data.Maybe (isNothing)

showFrames :: String -> IplImage -> Capture -> IO ()
showFrames win target cap  = do
  frame <- queryFrame cap
  convertImage frame target 0
  canny target target 30 190 3
  showImage win target
  k <- waitKey 5
  when (isNothing k) $ showFrames win target cap

main :: IO ()
main = do
  let win = "win"
  namedWindow win autoSize
  cap    <- createCameraCapture 0
  frame  <- queryFrame cap
  size   <- getSize frame
  target <- createImage size iplDepth8u 1
  showFrames win target cap
