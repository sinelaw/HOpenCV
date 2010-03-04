module Main where

import Foreign.Ptr
import Foreign.ForeignPtr

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

import Control.Monad(when, unless)
import Data.Maybe(fromJust, isJust)

showFrames :: Integral a => a -> Ptr IplImage -> Ptr CvCapture -> IO ()
showFrames winNum targetImage cvcapture  = do
  frame' <- cvQueryFrame cvcapture 
  case frame' of
    Nothing -> return ()
    Just frame  -> do 
      cvConvertImage (fromArr frame) (fromArr targetImage) 0
      cvCanny targetImage targetImage 30 150 3
      showImage winNum targetImage
      key <- waitKey (5::Int) :: IO Int
      unless (key /= -1) $ showFrames winNum targetImage cvcapture
  
processImages capture = do
  frame' <- cvQueryFrame capture
  case frame' of 
    Nothing -> return ()
    Just frame -> do
      let winNum = 0 :: Int
      newWindow winNum
      target' <- createImageF (cvGetSize frame) 1 IPL_DEPTH_8U
      case target' of
        Nothing -> return ()
        Just target -> withForeignPtr target (\target -> showFrames winNum target capture)
    
main :: IO ()
main = do
  capture' <- createCameraCaptureF (0 :: Int)
  case capture' of
    Nothing -> return ()
    Just capture -> withForeignPtr capture processImages


  