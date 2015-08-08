module Main where

import AI.CV.OpenCV
import Control.Monad (when)
import Data.Maybe (isNothing)

showFrames :: String -> IplImage -> Capture -> IO ()
showFrames win target cap  = do
  let step 0 = return ()
      step n = do
        frame <- queryFrame cap
        convertImage frame target 0
        canny target target 30 190 3
        showImage win target
        k <- waitKey 5
        putStr $ "\r" ++ show n ++ "\t\t frames before exiting...\r"
        when (isNothing k) $ step (n - 1)
  step 100

main :: IO ()
main = do
  putStrLn "Running..."
  let win = "win"
  namedWindow win autoSize
  cap    <- createCameraCapture 0
  frame  <- queryFrame cap
  size   <- getSize frame
  target <- createImage size iplDepth8u 1
  showFrames win target cap
