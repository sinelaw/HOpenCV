{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CV where

import Foreign.C.Types
import Foreign.Ptr
 
import AI.CV.OpenCV.CxCore


foreign import ccall unsafe "cv.h cvCanny"
  c_cvCanny :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> IO ()

cvCanny :: (IplArrayType i1, IplArrayType i2, Real a, Real b, Integral c) =>
           Ptr i1 -> Ptr i2 -> a -> b -> c -> IO ()
cvCanny src dst threshold1 threshold2 apertureSize = 
  c_cvCanny (fromArr src) (fromArr dst) (realToFrac threshold1) (realToFrac threshold2) (fromIntegral apertureSize)


data InterpolationMethod = CV_INTER_NN 
                         | CV_INTER_LINEAR 
                         | CV_INTER_CUBIC
                         | CV_INTER_AREA
                           deriving (Enum,Eq)

foreign import ccall unsafe "cv.h cvResize"
  c_cvResize :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvResize :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> InterpolationMethod -> IO ()
cvResize src dst interp = c_cvResize (fromArr src) (fromArr dst) (fromIntegral . fromEnum $ interp)

foreign import ccall unsafe "HOpenCV_warp.h dilate"
  c_dilate :: CInt -> Ptr CvArr -> Ptr CvArr -> IO ()

cvDilate :: (Integral a, IplArrayType i1, IplArrayType i2) => a -> Ptr i1 -> Ptr i2  -> IO ()
cvDilate iter src dst = c_dilate (fromIntegral iter) (fromArr src) (fromArr dst)

