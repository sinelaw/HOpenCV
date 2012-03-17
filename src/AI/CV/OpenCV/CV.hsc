{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CV where

import Foreign.C.Types
import Foreign.Ptr
 
import Data.Bits

import AI.CV.OpenCV.CxCore

#include <cv.h>


foreign import ccall unsafe "cv.h cvCanny"
  c_cvCanny :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CDouble -> CDouble -> CInt -> IO ()

cvCanny :: (IplArrayType i1, IplArrayType i2) =>
           i1 -> i2 -> CDouble -> CDouble -> CInt -> IO ()
cvCanny src dst threshold1 threshold2 apertureSize
  = let CvArr src' = fromArr src
        CvArr dst' = fromArr dst
    in c_cvCanny src' dst' (realToFrac threshold1) (realToFrac threshold2) apertureSize


data InterpolationMethod = CV_INTER_NN 
                         | CV_INTER_LINEAR 
                         | CV_INTER_CUBIC
                         | CV_INTER_AREA
                           deriving (Enum,Eq)

foreign import ccall unsafe "cv.h cvResize"
  c_cvResize :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CInt -> IO ()

cvResize :: (IplArrayType i1, IplArrayType i2) => i1 -> i2 -> InterpolationMethod -> IO ()
cvResize src dst interp
   = let CvArr src' = fromArr src
         CvArr dst' = fromArr dst
     in c_cvResize src' dst' (fromIntegral . fromEnum $ interp)

foreign import ccall unsafe "HOpenCV_wrap.h dilate"
  c_dilate :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CInt -> IO ()

cvDilate :: (IplArrayType i1, IplArrayType i2) => i1 -> i2  -> CInt -> IO ()
cvDilate src dst iter
  = let CvArr src' = fromArr src
        CvArr dst' = fromArr dst
    in c_dilate src' dst' iter


foreign import ccall unsafe "cv.h cvPyrDown"
  c_cvPyrDown :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CInt -> IO ()

-- for now only one filter type is supported so no need for the CInt (filter type)
constCvGaussian5x5 :: CInt
constCvGaussian5x5 = 7

cvPyrDown :: (IplArrayType i1, IplArrayType i2) => i1 -> i2 -> IO ()
cvPyrDown src dst
  = let CvArr src' = fromArr src
        CvArr dst' = fromArr dst
    in c_cvPyrDown src' dst' constCvGaussian5x5

------------------------------------------------------------------------------

data Priv_CvHaarClassifierCascade
type CvHaarClassifierCascade = Ptr Priv_CvHaarClassifierCascade

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
  c_cvHaarDetectObjects :: Ptr Priv_CvArr   -- ^ image
                        -> Ptr Priv_CvHaarClassifierCascade -- ^ cascade
                        -> Ptr Priv_CvMemStorage            -- ^ storage
                        -> CDouble                          -- ^ scale_factor
                        -> CInt                             -- ^ min_neighbors
                        -> CInt                             -- ^ flags
                        -> CInt -> CInt                     -- ^ min_size
                        -> IO (Ptr (Priv_CvSeq CvRect))

cvHaarDetectObjects :: (IplArrayType i) => 
                           i                       -- ^ image
                        -> CvHaarClassifierCascade -- ^ cascade
                        -> CvMemStorage            -- ^ storage
                        -> Double                 -- ^ scale_factor
                        -> Int                    -- ^ min_neighbors
                        -> HaarDetectFlag          -- ^ flags
                        -> CvSize                  -- ^ min_size
                        -> IO (CvSeq CvRect)
cvHaarDetectObjects image cascade storage scaleFactor minNeighbors flags minSize
  = let CvArr im = fromArr image
    in c_cvHaarDetectObjects im cascade storage 
                             (realToFrac scaleFactor)
                             (fromIntegral minNeighbors)
                             (unHaarDetectFlag flags)
                             (sizeWidth minSize) (sizeHeight minSize)
  
