{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CV where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
 
import Data.Bits

import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.Util

#include <cv.h>


foreign import ccall unsafe "cv.h cvCanny"
  c_cvCanny :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CDouble -> CDouble -> CInt -> IO ()

-- | Find edges using the Canny algorithm
canny :: IplImage -> IplImage -> Double -> Double -> Int -> IO ()
canny src dst threshold1 threshold2 apertureSize
  = withForeignPtr2 src dst $ \s d -> 
    c_cvCanny s d
              (realToFrac threshold1)
              (realToFrac threshold2)
              (fromIntegral apertureSize)

data InterpolationMethod = CV_INTER_NN 
                         | CV_INTER_LINEAR 
                         | CV_INTER_CUBIC
                         | CV_INTER_AREA
                           deriving (Enum,Eq)

foreign import ccall unsafe "cv.h cvResize"
  c_cvResize :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

-- | Resizes an image using the specified interpolation method
resize :: IplImage -> IplImage -> InterpolationMethod -> IO ()
resize src dst interp
   = withForeignPtr2 src dst $ \s d ->
      c_cvResize s d (fromIntegral . fromEnum $ interp)

foreign import ccall unsafe "HOpenCV_wrap.h dilate"
  c_dilate :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

-- | Dilates an image using a specific structuring element
dilate :: IplImage -> IplImage -> Int -> IO ()
dilate src dst iter
  = withForeignPtr2 src dst
     $ \s d -> c_dilate s d $ fromIntegral iter


foreign import ccall unsafe "cv.h cvPyrDown"
  c_cvPyrDown :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

-- for now only one filter type is supported so no need for the CInt (filter type)
constCvGaussian5x5 :: CInt
constCvGaussian5x5 = 7

-- | Smooths an image and downsamples it, currently only gaussian5x5 is supported
pyrDown :: IplImage -> IplImage -> IO ()
pyrDown src dst
  = withForeignPtr2 src dst
     $ \s d -> c_cvPyrDown s d constCvGaussian5x5

------------------------------------------------------------------------------

data Priv_CvHaarClassifierCascade
type HaarClassifierCascade = ForeignPtr Priv_CvHaarClassifierCascade

-- thanks to http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
newtype HaarDetectFlag = HaarDetectFlag { unHaarDetectFlag :: CInt }
    deriving (Eq, Show)
             
#{enum HaarDetectFlag, HaarDetectFlag
 , haarFlagNone = 0
 , haarDoCannyPruning    = CV_HAAR_DO_CANNY_PRUNING 
 , haarScaleImage        = CV_HAAR_SCALE_IMAGE 
 , haarFindBiggestObject = CV_HAAR_FIND_BIGGEST_OBJECT 
 , haarDoRoughSearch     = CV_HAAR_DO_ROUGH_SEARCH
 }

combineHaarFlags :: [HaarDetectFlag] -> HaarDetectFlag
combineHaarFlags = HaarDetectFlag . foldr ((.|.) . unHaarDetectFlag) 0
  
foreign import ccall unsafe "HOpenCV_wrap.h c_cvHaarDetectObjects"
  c_cvHaarDetectObjects :: Ptr Priv_IplImage -- ^ image
                        -> Ptr Priv_CvHaarClassifierCascade -- ^ cascade
                        -> Ptr Priv_CvMemStorage            -- ^ storage
                        -> CDouble                          -- ^ scale_factor
                        -> CInt                             -- ^ min_neighbors
                        -> CInt                             -- ^ flags
                        -> CInt -> CInt                     -- ^ min_size
                        -> IO (Ptr (Priv_CvSeq CvRect))

-- | Detects objects in the image. Matches are returned as a list of rectangles
haarDetectObjects :: IplImage              -- ^ image
                  -> HaarClassifierCascade -- ^ cascade
                  -> MemStorage            -- ^ storage
                  -> Double                -- ^ scale_factor
                  -> Int                   -- ^ min_neighbors
                  -> HaarDetectFlag        -- ^ flags
                  -> CvSize                -- ^ min_size
                  -> IO (CvSeq CvRect)
haarDetectObjects image cascade storage scaleFactor minNeighbors flags minSize
  = do p  <- withForeignPtr3 image storage cascade $ \im' s' c' -> 
             c_cvHaarDetectObjects im' c' s'
                                   (realToFrac scaleFactor)
                                   (fromIntegral minNeighbors)
                                   (unHaarDetectFlag flags)
                                   (sizeWidth minSize) (sizeHeight minSize)
       newForeignPtr cvFree p 
  
