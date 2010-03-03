module Main where

import Foreign.Ptr
import Foreign.ForeignPtr

import AI.CV.OpenCV.HighGui

import Control.Monad(unless)


showFrames :: Integral a => a -> Ptr CvCapture -> IO ()
showFrames winNum cvcapture  = do
  frame <- cvQueryFrame cvcapture 
  case frame of
    Nothing -> return ()
    Just f  -> do 
      showImage winNum f
      key <- waitKey (5::Int) :: IO Int
      unless (key /= -1) $ showFrames winNum cvcapture
  
main :: IO ()
main = do
  let winNum = 0 :: Int
  newWindow winNum
  capture <- createCameraCapture (0 :: Int)
  case capture of
     Nothing -> return ()
     Just ptr -> withForeignPtr ptr (showFrames winNum)


  