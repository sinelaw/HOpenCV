{-# LANGUAGE ForeignFunctionInterface #-}
-- |Image filtering operations.
module AI.CV.OpenCV.Filtering (smoothGaussian, smoothGaussian') where
import Foreign.C.Types (CInt, CDouble)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtil

#include <opencv2/imgproc/types_c.h>

foreign import ccall unsafe "opencv2/imgproc/imgproc_c.h cvSmooth"
  c_cvSmooth :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> CInt -> CDouble -> 
                CDouble -> IO ()

smooth :: Ptr IplImage -> Ptr IplImage -> CInt -> Int -> Int -> Double -> 
          Double -> IO ()
smooth src dst smoothType param1 param2 param3 param4 = 
    c_cvSmooth (castPtr src) (castPtr dst) smoothType (fi param1)
               (fi param2) (rf param3) (rf param4)
    where fi = fromIntegral
          rf = realToFrac

cvGaussian :: CInt
cvGaussian = #{const CV_GAUSSIAN}

-- |Smooth a source image using a linear convolution with a Gaussian
-- kernel. Parameters are the kernel width and the source
-- 'HIplImage'. The kernel height will be set to the same value as the
-- width, and the Gaussian standard deviation will be calculated from
-- the kernel size. This function is the same as calling
-- @smoothGaussian' width Nothing Nothing@. May be performed in-place
-- under composition.
smoothGaussian :: (ByteOrFloat d, HasChannels c) => 
                  Int -> HIplImage c d -> HIplImage c d
smoothGaussian w = smoothGaussian' w Nothing Nothing
{-# INLINE [0] smoothGaussian #-}

-- |Smooth a source 'HIplImage' using a linear convolution with a
-- Gaussian kernel. Parameters are the kernel width, the kernel height
-- (if 'Nothing', the height will be set to the same value as the
-- width), the Gaussian standard deviation (if 'Nothing', it will be
-- calculated from the kernel size), and the source image. May be
-- performed in-place under composition.
smoothGaussian' :: (ByteOrFloat d, HasChannels c) => 
                   Int -> Maybe Int -> Maybe Double -> HIplImage c d -> 
                   HIplImage c d
smoothGaussian' w h sigma src = 
    unsafePerformIO $
    withHIplImage src $ \src' ->
      return . fst . withCompatibleImage src $ \dst ->
        smooth src' dst cvGaussian w h' sigma' 0
    where sigma' = case sigma of { Nothing -> 0; Just s -> s }
          h' = case h of { Nothing -> 0; Just jh -> jh }
{-# INLINE [0] smoothGaussian' #-}

unsafeGaussian'  :: (ByteOrFloat d, HasChannels c) => 
                    Int -> Maybe Int -> Maybe Double ->
                    HIplImage c d -> IO (HIplImage c d)
unsafeGaussian' w h sigma src = withHIplImage src $ \src' ->
                                  do smooth src' src' cvGaussian w h' sigma' 0
                                     return src
    where sigma' = case sigma of
                     Nothing -> 0
                     Just s -> realToFrac s
          h' = case h of { Nothing -> 0; Just jh -> jh }
{-# INLINE [0] unsafeGaussian' #-}

unsafeGaussian  :: (ByteOrFloat d, HasChannels c) => 
                    Int -> HIplImage c d -> IO (HIplImage c d)
unsafeGaussian w = unsafeGaussian' w Nothing Nothing
{-# INLINE [0] unsafeGaussian #-}

{-# RULES 
"smoothGaussian'/in-place" [~1] forall w h sigma. 
  smoothGaussian' w h sigma = pipeline (unsafeGaussian' w h sigma)
"smoothGaussian'/unpipe" [1] forall w h sigma.
  pipeline (unsafeGaussian' w h sigma) = smoothGaussian' w h sigma
"smoothGaussian/in-place" [~1] forall w. 
  smoothGaussian w = pipeline (unsafeGaussian w)
"smoothGaussian/unpipe" [1] forall w.
  pipeline (unsafeGaussian w) = smoothGaussian w
  #-}
