{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CV where

import Foreign.C.Types
import Foreign.Ptr
 
import Data.Bits

import AI.CV.OpenCV.CxCore

#include <cv.h>


foreign import ccall unsafe "cv.h cvCanny"
  c_cvCanny :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> IO ()

cvCanny :: (IplArrayType i1, IplArrayType i2) =>
           Ptr i1 -> Ptr i2 -> CDouble -> CDouble -> CInt -> IO ()
cvCanny src dst threshold1 threshold2 apertureSize = 
  c_cvCanny (fromArr src) (fromArr dst) (realToFrac threshold1) (realToFrac threshold2) apertureSize


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
  c_dilate :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvDilate :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2  -> CInt -> IO ()
cvDilate src dst iter = c_dilate (fromArr src) (fromArr dst) iter


foreign import ccall unsafe "cv.h cvPyrDown"
  c_cvPyrDown :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

-- for now only one filter type is supported so no need for the CInt (filter type)
constCvGaussian5x5 :: CInt
constCvGaussian5x5 = 7
cvPyrDown :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> IO ()
cvPyrDown src dst = c_cvPyrDown (fromArr src) (fromArr dst) constCvGaussian5x5

------------------------------------------------------------------------------

data CvHaarClassifierCascade

-- thanks to http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
newtype HaarDetectFlag = HaarDetectFlag { unHaarDetectFlag :: CInt }
    deriving (Eq, Show)
             
#{enum HaarDetectFlag, HaarDetectFlag
  , cvHaarDoCannyPruning    = CV_HAAR_DO_CANNY_PRUNING 
  , cvHaarScaleImage        = CV_HAAR_SCALE_IMAGE 
  , cvHaarFindBiggestObject = CV_HAAR_FIND_BIGGEST_OBJECT 
  , cvHaarDoRoughSearch     = CV_HAAR_DO_ROUGH_SEARCH
 }

combineHaarFlags :: [HaarDetectFlag] -> HaarDetectFlag
combineHaarFlags = HaarDetectFlag . foldr ((.|.) . unHaarDetectFlag) 0
  
foreign import ccall unsafe "HOpenCV_warp.h c_cvHaarDetectObjects"
  c_cvHaarDetectObjects :: Ptr CvArr   -- ^ image
                        -> Ptr CvHaarClassifierCascade -- ^ cascade
                        -> Ptr CvMemStorage            -- ^ storage
                        -> CDouble                     -- ^ scale_factor
                        -> CInt                        -- ^ min_neighbors
                        -> CInt                        -- ^ flags
                        -> CInt -> CInt                -- ^ min_size
                        -> IO (Ptr (CvSeq CvRect))

cvHaarDetectObjects :: (IplArrayType i) => 
                           Ptr i                       -- ^ image
                        -> Ptr CvHaarClassifierCascade -- ^ cascade
                        -> Ptr CvMemStorage            -- ^ storage
                        -> CDouble                     -- ^ scale_factor
                        -> CInt                        -- ^ min_neighbors
                        -> HaarDetectFlag              -- ^ flags
                        -> CvSize                      -- ^ min_size
                        -> IO (Ptr (CvSeq CvRect))
cvHaarDetectObjects image cascade storage scaleFactor minNeighbors flags minSize = 
  c_cvHaarDetectObjects (fromArr image) cascade storage scaleFactor minNeighbors (unHaarDetectFlag flags) (sizeWidth minSize) (sizeHeight minSize)
  