{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- |Support for features from the OpenCV Image Filtering library.
module AI.CV.OpenCV.CV 
    ( InterpolationMethod(..),
      cvCanny, cvResize, cvDilate, cvErode, cvPyrDown, cvHoughLines2, 
      CvHaarClassifierCascade, HaarDetectFlag,
      cvHaarFlagNone, cvHaarDoCannyPruning, 
      cvHaarScaleImage, cvHaarFindBiggestObject, cvHaarDoRoughSearch,
      combineHaarFlags, cvHaarDetectObjects,
      cvCvtColor
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Data.Bits
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.ColorConversion

#include <opencv/cv.h>

foreign import ccall unsafe "opencv/cv.h cvCanny"
  c_cvCanny :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> IO ()

-- Canny 
cvCanny :: (IplArrayType i1, IplArrayType i2) =>
           Ptr i1 -> Ptr i2 -> CDouble -> CDouble -> CInt -> IO ()
cvCanny src dst threshold1 threshold2 apertureSize = 
  c_cvCanny (fromArr src) (fromArr dst) (realToFrac threshold1) 
            (realToFrac threshold2) apertureSize


data InterpolationMethod = CV_INTER_NN 
                         | CV_INTER_LINEAR 
                         | CV_INTER_CUBIC
                         | CV_INTER_AREA
                           deriving (Enum,Eq)

foreign import ccall unsafe "opencv/cv.h cvResize"
  c_cvResize :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvResize :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> InterpolationMethod -> IO ()
cvResize src dst interp = c_cvResize (fromArr src) (fromArr dst) (fromIntegral . fromEnum $ interp)

foreign import ccall unsafe "opencv/cv.h cvDilate"
  c_dilate :: Ptr CvArr -> Ptr CvArr -> Ptr () -> CInt -> IO ()

-- |Dilate the first image using a 3x3 rectangular structuring element
-- and store the result in the second image. The third parameter is
-- the number of dilation iterations to perform.
cvDilate :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2  -> CInt -> IO ()
cvDilate src dst iter = c_dilate (fromArr src) (fromArr dst) nullPtr iter

foreign import ccall unsafe "opencv/cv.h cvErode"
  c_erode :: Ptr CvArr -> Ptr CvArr -> Ptr () -> CInt -> IO ()

-- |Erode the first image using a 3x3 rectangular structuring element
-- and store the result in the second image. The third parameter is
-- the number of erosion iterations to perform.
cvErode :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> CInt -> IO ()
cvErode src dst iter = c_erode (fromArr src) (fromArr dst) nullPtr iter

foreign import ccall unsafe "opencv/cv.h cvHoughLines2"
        c_cvHoughLines2 :: Ptr CvArr -> Ptr CvMemStorage -> CInt -> CDouble -> CDouble -> CInt -> CDouble -> CDouble -> IO (Ptr (CvSeq a))

cvHoughLines2 :: IplArrayType i => Ptr i -> Ptr CvMemStorage -> CInt -> Double -> Double -> Int -> Double -> Double -> IO (Ptr (CvSeq a))
cvHoughLines2 img storage method rho theta threshold param1 param2 = 
    c_cvHoughLines2 (fromArr img) storage method (realToFrac rho) 
                    (realToFrac theta) (fromIntegral threshold) 
                    (realToFrac param1) (realToFrac param2)

foreign import ccall unsafe "opencv/cv.h cvCvtColor"
  c_cvCvtColor :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

-- |Convert the color of the first 'IplImage', storing the result in
-- the second. The second image must have the same dimensions as the
-- first and the same depth, but it's number of color channels may be
-- different and must be compatible with the given 'ColorConversion'
-- code.
cvCvtColor :: (IplArrayType a, IplArrayType b) => 
              Ptr a -> Ptr b -> ColorConversion -> IO ()
cvCvtColor src dst code = c_cvCvtColor (fromArr src) (fromArr dst) (colorConv code)

foreign import ccall unsafe "opencv/cv.h cvPyrDown"
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
 , cvHaarFlagNone = 0
 , cvHaarDoCannyPruning    = CV_HAAR_DO_CANNY_PRUNING 
 , cvHaarScaleImage        = CV_HAAR_SCALE_IMAGE 
 , cvHaarFindBiggestObject = CV_HAAR_FIND_BIGGEST_OBJECT 
 , cvHaarDoRoughSearch     = CV_HAAR_DO_ROUGH_SEARCH
 }

combineHaarFlags :: [HaarDetectFlag] -> HaarDetectFlag
combineHaarFlags = HaarDetectFlag . foldr ((.|.) . unHaarDetectFlag) 0
  
foreign import ccall unsafe "HOpenCV_wrap.h c_cvHaarDetectObjects"
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
  