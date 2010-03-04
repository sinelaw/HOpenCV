{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CV where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
 
import AI.CV.OpenCV.CxCore


foreign import ccall unsafe "cv.h cvCanny"
  c_cvCanny :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> IO ()

cvCanny :: (IplArrayType i1, IplArrayType i2, Real a, Real b, Integral c) =>
           Ptr i1 -> Ptr i2 -> a -> b -> c -> IO ()
cvCanny inArr outArr threshold1 threshold2 apertureSize = 
  c_cvCanny (fromArr inArr) (fromArr outArr) (realToFrac threshold1) (realToFrac threshold2) (fromIntegral apertureSize)
