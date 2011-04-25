{-# LANGUAGE ForeignFunctionInterface #-}
-- |Feature Detection.
module AI.CV.OpenCV.FeatureDetection (cornerHarris, cornerHarris', canny) where
import Foreign.C.Types (CInt, CDouble)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtils

foreign import ccall unsafe "opencv2/imgproc/imgproc_c.h cvCornerHarris"
  c_cvHarris :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> CDouble -> IO ()

harris :: Ptr IplImage -> Ptr IplImage -> Int -> Int -> Double -> IO ()
harris src dst blockSize aperture k = 
    c_cvHarris (castPtr src) (castPtr dst) (fi blockSize) (fi aperture) (rf k)
    where fi = fromIntegral
          rf = realToFrac

-- |Harris corner detector. For each pixel, a 2x2 covariance matrix,
-- @M@, is computed over a @blockSize x blockSize@ neighborhood. The
-- value of @det(M) - 0.04*trace(M)^2@ is stored in the destination
-- image. Corners in the image correspond to local maxima of the
-- destination image. The parameters are the @blockSize@ and the
-- source 'HIplImage'. The Sobel operator used as a preprocessing step
-- is given an aperture size of 3.
cornerHarris :: ByteOrFloat d => 
                Int -> HIplImage MonoChromatic d -> 
                HIplImage MonoChromatic Float
cornerHarris blockSize = cornerHarris' blockSize 3 0.04

-- |Harris corner detector. For each pixel, a 2x2 covariance matrix,
-- @M@, is computed over a @blockSize x blockSize@ neighborhood. The
-- value of @det(M) - k*trace(M)^2@ is stored in the destination
-- image. Corners in the image correspond to local maxima of the
-- destination image. The parameters are the @blockSize@, the
-- @aperture@ size to be used by the Sobel operator that is run during
-- corner evaluation, the value of @k@, and the source
-- 'HIplImage'.
cornerHarris' :: ByteOrFloat d => 
                 Int -> Int -> Double -> HIplImage MonoChromatic d -> 
                 HIplImage MonoChromatic Float
cornerHarris' blockSize aperture k src = 
    unsafePerformIO $ do dst <- mkHIplImage (width src) (height src)
                         withHIplImage src $ \src' ->
                           withHIplImage dst $ \dst' ->
                             harris src' dst' blockSize aperture k
                         return dst

foreign import ccall unsafe "opencv2/imgprog/imgproc_c.h cvCanny"
  c_cvCanny :: Ptr IplImage -> Ptr IplImage -> CDouble -> CDouble -> CInt -> IO ()

-- |Canny edge detector. @canny threshold1 threshold2 aperture src@
-- finds edges in the source image with the larger of the two
-- thresholds used to find initial segments of strong edges while the
-- smaller threshold is used for edge linking. The third parameter is
-- the aperture size used for initial Sobel operator edge detection.
canny :: HasDepth d =>
         Double -> Double -> Int -> HIplImage MonoChromatic d -> 
         HIplImage MonoChromatic d
canny t1 t2 aperture src = 
  unsafeWithHIplImage src $ \src' ->
    fst . withCompatibleImage src $ \dst ->
          c_cvCanny src' dst (rf t1) (rf t2) (fi aperture)
  where rf = realToFrac
        fi = fromIntegral
    