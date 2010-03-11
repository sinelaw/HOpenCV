module Main where

import Foreign.Ptr
import Foreign.ForeignPtr

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

import Control.Monad(when)

showFrames :: Integral a => a -> Ptr IplImage -> Ptr CvCapture -> IO ()
showFrames winNum targetImage cvcapture  = do
  frame <- cvQueryFrame $ cvcapture 
  cvConvertImage (fromArr frame) (fromArr targetImage) $ 0
  ts <- createImageF (CvSize 320 240) (1::Int) $ IPL_DEPTH_8U
  withForeignPtr ts $ calcFrame
      where calcFrame targetSmall = do
              cvResize targetImage targetSmall CV_INTER_LINEAR
              cvCanny targetSmall targetSmall (30::Int) (150::Int) (3::Int)
              showImage winNum targetSmall
              key <- waitKey (5::Int) :: IO Int
              when (key == -1) (showFrames winNum targetImage cvcapture)

  
processImages :: Ptr CvCapture -> IO ()
processImages capture = do
  frame <- cvQueryFrame $ capture
  let winNum = 0 :: Int
  newWindow $ winNum
  target <- createImageF (cvGetSize frame) (1::Int) $ IPL_DEPTH_8U
  withForeignPtr target $ (\target' -> showFrames winNum target' capture) 
    
main :: IO ()
main = do
  capture <- createCameraCaptureF $ (0 :: Int)
  withForeignPtr capture processImages


  