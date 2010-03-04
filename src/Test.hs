module Main where

import Foreign.Ptr
import Foreign.ForeignPtr

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighGui

import Control.Monad(when)
import Control.Monad.Trans(lift)
import Control.Monad.Maybe

showFrames :: Integral a => a -> Ptr IplImage -> Ptr CvCapture -> MaybeT IO ()
showFrames winNum targetImage cvcapture  = do
  frame <- MaybeT . cvQueryFrame $ cvcapture 
  lift . cvConvertImage (fromArr frame) (fromArr targetImage) $ 0
  ts <- MaybeT . createImageF (CvSize 320 240) (1::Int) $ IPL_DEPTH_8U
  lift . withForeignPtr ts $ calcFrame
      where calcFrame targetSmall = do
              cvResize targetImage targetSmall CV_INTER_LINEAR
              cvCanny targetSmall targetSmall (50::Int) (180::Int) (3::Int)
              showImage winNum targetSmall
              key <- waitKey (5::Int) :: IO Int
              when (key == -1) (runMaybeT (showFrames winNum targetImage cvcapture) >> return ())

  
processImages :: Ptr CvCapture -> MaybeT IO ()
processImages capture = do
  frame <- MaybeT . cvQueryFrame $ capture
  let winNum = 0 :: Int
  lift . newWindow $ winNum
  target <- MaybeT . createImageF (cvGetSize frame) (1::Int) $ IPL_DEPTH_8U
  lift . withForeignPtr target $ (\target' -> runMaybeT (showFrames winNum target' capture) >> return ())
    
main' :: MaybeT IO ()
main' = do
  capture <- MaybeT . createCameraCaptureF $ (0 :: Int)
  lift . withForeignPtr capture $ (\capture' -> runMaybeT (processImages capture') >> return ())
  
main :: IO ()
main = do
  runMaybeT main'
  return ()


  