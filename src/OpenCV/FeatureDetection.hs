{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts, DataKinds #-}
-- |Feature Detection.
module OpenCV.FeatureDetection (cornerHarris, cornerHarris', canny) where
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr, castPtr)
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp

foreign import ccall "opencv2/imgproc/imgproc_c.h cvCornerHarris"
  c_cvHarris :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> CDouble -> IO ()

harris :: Ptr IplImage -> Ptr IplImage -> Int -> Int -> Double -> IO ()
harris src dst blockSize aperture k = 
    c_cvHarris (castPtr src) (castPtr dst) (fi blockSize) (fi aperture) (rf k)
    where fi = fromIntegral
          rf = realToFrac

type M = Monochromatic

-- |Equivalent to 'cornerHarris'' with an @aperture@ of @3@ and a @k@
-- of @0.04@.
cornerHarris :: (ByteOrFloat d, Inplace r M d M Float) => 
                Int -> Image Monochromatic d r -> 
                Image Monochromatic Float r
cornerHarris blockSize = cornerHarris' blockSize 3 0.04
{-# INLINE cornerHarris #-}

-- |Harris corner detector. For each pixel, a 2x2 covariance matrix,
-- @M@, is computed over a @blockSize x blockSize@ neighborhood. The
-- value of @det(M) - k*trace(M)^2@ is stored in the destination
-- image. Corners in the image correspond to local maxima of the
-- destination image. The parameters are the @blockSize@, the
-- @aperture@ size to be used by the Sobel operator that is run during
-- corner evaluation, the value of @k@, and the source
-- 'Image'.
cornerHarris' :: (ByteOrFloat d, Inplace r M d M Float) => 
                 Int -> Int -> Double -> Image Monochromatic d r -> 
                 Image Monochromatic Float r
cornerHarris' blockSize aperture k = 
    cv2 $ \src dst -> harris src dst blockSize aperture k
{-# INLINE cornerHarris' #-}

foreign import ccall "opencv2/imgprog/imgproc_c.h cvCanny"
  c_cvCanny :: Ptr IplImage -> Ptr IplImage -> CDouble -> CDouble -> CInt -> IO ()

-- |Canny edge detector. @canny threshold1 threshold2 aperture src@
-- finds edges in the source image with the larger of the two
-- thresholds used to find initial segments of strong edges while the
-- smaller threshold is used for edge linking. The third parameter is
-- the aperture size used for initial Sobel operator edge detection.
canny :: (HasDepth d, Inplace r M d M d) =>
         Double -> Double -> Int -> Image Monochromatic d r -> 
         Image Monochromatic d r
canny t1 t2 aperture = 
    cv2 $ \src dst -> c_cvCanny src dst (rf t1) (rf t2) (fi aperture)
    --cv $ \src -> c_cvCanny src src (rf t1) (rf t2) (fi aperture)
  where rf = realToFrac
        fi = fromIntegral
