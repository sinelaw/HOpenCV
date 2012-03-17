module Main where

import Foreign.ForeignPtr

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

import Control.Monad(when)

showFrames :: Int -> IplImage -> CvCapture -> IO ()
showFrames winNum targetImage cvcapture  = do
  frame <- cvQueryFrame cvcapture 
  -- let CvArr frame'  = fromArr frame
      -- CvArr target' = fromArr targetImage
  cvConvertImage frame targetImage 0
  calcFrame targetImage
      where calcFrame targetSmall = do
              cvResize targetImage targetSmall CV_INTER_LINEAR
              cvCanny targetSmall targetSmall 30 190 3
              showImage winNum targetSmall
              key <- waitKey 5
              when (key == -1) (showFrames winNum targetImage cvcapture)

  
processImages :: CvCapture -> IO ()
processImages capture = do
  frame <- cvQueryFrame capture
  let winNum = 0
  newWindow winNum True
  IplImageF target <- createImageF (cvGetSize frame) 1 iplDepth8u
  withForeignPtr target $ \target' -> showFrames winNum (IplImage target') capture
    
main :: IO ()
main = do
  capture <- createCameraCaptureF 0
  withForeignPtr capture processImages


  
