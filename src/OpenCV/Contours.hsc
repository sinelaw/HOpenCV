{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |Incomplete support for cvFindContours.
module OpenCV.Contours (
                        
  -- * contour
  Contour(..),
  withContourList,
  -- ** functions to use with 'withContourList'
  cvContourArea,
  cvContourPerimeter,
  followPoints,

  cvDrawContours,
  -- * moments
  cvMoments,
  CvMoments(..),

  cvGetSpatialMoment,
  cvGetCentralMoment,
  cvGetNormalizedCentralMoment,
  cvGetHuMoments,
  CvHuMoments(..),

  -- * unused
  ContourMode(..), ContourMethod(..), 
  ) where
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import Foreign.C.Types (CInt(..))
import Foreign.Ptr -- (Ptr, castPtr, nullPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc -- (alloca)
import Foreign.ForeignPtr.Safe
import Foreign.Marshal

import Control.Monad

#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/types_c.h>

foreign import ccall "HOpenCV_wrap.h c_cvFindContours"
  c_cvFindContours :: Ptr CvArr -> Ptr CvMemStorage -> Ptr (Ptr (CvSeq a)) ->
                      CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "HOpenCV_wrap.h c_cvGetSeqPoint"
  c_cvGetSeqPoint :: Ptr (CvSeq a) -> CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall "HOpenCV_wrap.h c_cvContourArea"
  cvContourArea :: Contour -> IO Double

foreign import ccall "HOpenCV_wrap.h c_cvContourPerimeter"
  cvContourPerimeter :: Contour -> IO Double

foreign import ccall "HOpenCV_wrap.h c_cvMoments"
  c_cvMoments :: Ptr CvArr -> Ptr CvMoments -> IO ()

cvMoments :: IplArrayType a => Ptr a -> IO CvMoments
cvMoments img = do
    p <- mallocForeignPtrBytes (# size CvMoments )
    withForeignPtr p (c_cvMoments (castPtr img))
    return (CvMoments p)

foreign import ccall "cvGetSpatialMoment" 
  c_cvGetSpatialMoment :: Ptr CvMoments -> CInt -> CInt -> IO Double

cvGetSpatialMoment, cvGetCentralMoment, cvGetNormalizedCentralMoment
  ::  CvMoments
  -> CInt -- ^ x
  -> CInt -- ^ y
  -> IO Double
cvGetSpatialMoment (CvMoments p) xord yord =
    withForeignPtr p (\ p' -> c_cvGetSpatialMoment p' xord yord )

foreign import ccall "cvGetCentralMoment" 
  c_cvGetCentralMoment :: Ptr CvMoments -> CInt -> CInt -> IO Double

cvGetCentralMoment (CvMoments p) xord yord =
    withForeignPtr p (\ p' -> c_cvGetCentralMoment p' xord yord )

foreign import ccall "cvGetNormalizedCentralMoment" 
  c_cvGetNormalizedCentralMoment :: Ptr CvMoments -> CInt -> CInt -> IO Double

cvGetNormalizedCentralMoment (CvMoments p) xord yord =
    withForeignPtr p (\ p' -> c_cvGetNormalizedCentralMoment p' xord yord )

foreign import ccall "cvGetHuMoments" 
  c_cvGetHuMoments :: Ptr CvMoments -> Ptr () -> IO ()

foreign import ccall "c_cvDrawContours"
  c_cvDrawContours :: Ptr IplImage
    -> Contour
    -> Ptr CvScalar -- ^ RGBA external_color
    -> Ptr CvScalar -- ^ RGBA hole_color
    -> CInt -- max level
    -> CInt -- thickness
    -> CInt -- line-type
    -> Ptr CvPoint -- offset
    -> IO ()

cvDrawContours ::
   Ptr IplImage
   -> Contour
   -> CvScalar -- ^ external color
   -> CvScalar -- ^ hole color
   -> CInt -- ^ max level
   -> CInt -- ^ thickness
   -> CInt -- ^ line type
   -> CvPoint -- ^ offset
   -> IO ()
cvDrawContours img contour external_color hole_color max_level thickness line_type offset =
    with external_color $ \ec ->
    with hole_color $ \ hc ->
    with offset $ \offset' ->
    c_cvDrawContours img contour ec hc max_level thickness line_type offset'


cvGetHuMoments :: CvMoments -> IO CvHuMoments
cvGetHuMoments (CvMoments m) = allocaBytes (# size CvHuMoments )  $ \p -> do
    withForeignPtr m $ \m' -> c_cvGetHuMoments m' p
    h1 <- (#peek CvHuMoments, hu1) p
    h2 <- (#peek CvHuMoments, hu2) p
    h3 <- (#peek CvHuMoments, hu3) p
    h4 <- (#peek CvHuMoments, hu4) p
    h5 <- (#peek CvHuMoments, hu5) p
    h6 <- (#peek CvHuMoments, hu6) p
    h7 <- (#peek CvHuMoments, hu7) p
    return (CvHuMoments h1 h2 h3 h4 h5 h6 h7)

newtype Contour = Contour (Ptr (CvSeq (CInt, CInt)))

-- | abstract. Use 'cvGetSpatialMoment' etc.
newtype CvMoments = CvMoments (ForeignPtr CvMoments)

data CvHuMoments = CvHuMoments !Double !Double !Double !Double !Double !Double !Double
                 deriving Show

-- |Contour extraction mode.
data ContourMode = CV_RETR_EXTERNAL -- ^retrieves only the extreme
                                    -- outer contours

                 | CV_RETR_LIST     -- ^retrieves all of the contours
                                    -- and puts them in the list

                 | CV_RETR_CCOMP    -- ^retrieves all of the contours
                                    -- and organizes them into a
                                    -- two-level hierarchy: on the top
                                    -- level are the external
                                    -- boundaries of the components,
                                    -- on the second level are the
                                    -- boundaries of the holes

                 | CV_RETR_TREE     -- ^retrieves all of the contours
                                    -- and reconstructs the full
                                    -- hierarchy of nested contours
                 | CV_RETR_FLOODFILL
                   deriving (Enum, Eq)

data ContourMethod = CV_CHAIN_CODE -- ^ ??
                   | CV_CHAIN_APPROX_NONE      
                   -- ^translates all of the points from the chain
                   -- code into points

                   | CV_CHAIN_APPROX_SIMPLE    
                   -- ^compresses horizontal, vertical, and diagonal
                   -- segments and leaves only their end points

                   | CV_CHAIN_APPROX_TC89_L1   
                   -- ^applies one of the flavors of the Teh-Chin
                   -- chain approximation algorithm

                   | CV_CHAIN_APPROX_TC89_KCOS
                   -- ^applies one of the flavors of the Teh-Chin
                   -- chain approximation algorithm

                   | CV_LINK_RUNS
                   -- ^uses a completely different contour retrieval
                   -- algorithm by linking horizontal segments of
                   -- 1's. Only the CV_RETR_LIST retrieval mode can be
                   -- used with this method.  
                     deriving Enum

-- |The function retrieves 'CvContour's from the binary image using the
-- algorithm Suzuki85. The contours are a useful tool for shape
-- analysis and object detection and recognition.
--
-- only does @CV_RETR_LIST CV_CHAIN_APPROX_SIMPLE@
--
withContourList :: Image Monochromatic d roi -> (Contour -> IO r) -> IO [r]
withContourList img fn = withIplImage img $ \imgPtr -> do
       storage <- cvCreateMemStorage 0
       cseq <- new nullPtr
       n <- c_cvFindContours (fromArr imgPtr) storage
                       cseq
                       (#size CvContour)
                       (fromIntegral (fromEnum CV_RETR_LIST))
                       (fromIntegral (fromEnum CV_CHAIN_APPROX_SIMPLE)) 0 0
       cs <- mapContours fn n . Contour =<< peek cseq
       cvReleaseMemStorage storage
       free cseq
       return cs

mapContours :: (Contour -> IO b) -> CInt -> Contour -> IO [ b ]
mapContours fn n (Contour p0) = go [] n p0 where
  go acc 0 p = return $ reverse acc
  go acc n p | p == nullPtr = return $ reverse acc
  go acc n p = do
        b <- fn (Contour p)
        p' <- (#peek CvSeq, h_next) p
        go (b:acc) (n-1) p'

followPoints :: Contour -> IO [(CInt, CInt)]
followPoints (Contour p) = do
  m <- (#peek CvSeq, total) p
  alloca $ \x -> alloca $ \y ->
    let go (-1) acc = return acc
        go ni acc = do
          c_cvGetSeqPoint p ni x y
          xy <- liftM2 (,) (peek x) (peek y)
          go (ni-1) (xy : acc)
    in go ((m::CInt) - 1) []

