module Main where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

import Control.Monad(when)

showFrames :: CInt -> Ptr IplImage -> Ptr CvCapture -> IO ()
showFrames winNum targetImage cvcapture  = do
  frame <- cvQueryFrame cvcapture 
  cvConvertImage (fromArr frame) (fromArr targetImage) 0
  
  ts <- createImageF (CvSize 320 240) 1 iplDepth8u
  withForeignPtr ts $ calcFrame
      where calcFrame targetSmall = do
              cvResize targetImage targetSmall CV_INTER_LINEAR
              cvCanny targetSmall targetSmall 30 190 3
              showImage winNum targetSmall
              key <- waitKey 5
              when (key == -1) (showFrames winNum targetImage cvcapture)

  
processImages :: Ptr CvCapture -> IO ()
processImages capture = do
  frame <- cvQueryFrame capture
  let winNum = 0
  newWindow winNum 0
  target <- createImageF (cvGetSize frame) 1 iplDepth8u
  withForeignPtr target $ (\target' -> showFrames winNum target' capture) 
    
main :: IO ()
main = do
  capture <- createCameraCaptureF 0
  withForeignPtr capture processImages


  