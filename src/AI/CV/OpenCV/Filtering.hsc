{-# LANGUAGE ForeignFunctionInterface #-}
-- |Image filtering operations.
module AI.CV.OpenCV.Filtering (smoothGaussian, smoothGaussian') where
import Foreign.C.Types (CInt, CDouble)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.HIplUtils

#include <opencv/cv.h>

foreign import ccall unsafe "opencv/cv.h cvSmooth"
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
-- @smoothGaussian' width Nothing Nothing@. This operation may be
-- performed in-place under composition.
smoothGaussian :: (ByteOrFloat d, HasDepth d, Storable d, HasChannels c) => 
                  Int -> HIplImage a c d -> HIplImage FreshImage c d
smoothGaussian w = smoothGaussian' w Nothing Nothing

-- |Smooth a source 'HIplImage' using a linear convolution with a
-- Gaussian kernel. Parameters are the kernel width, the kernel height
-- (if 'Nothing', the height will be set to the same value as the
-- width), the Gaussian standard deviation (if 'Nothing', it will be
-- calculated from the kernel size), and the source image. This
-- operation may be performed in-place under composition.
smoothGaussian' :: (ByteOrFloat d, HasDepth d, Storable d, HasChannels c) => 
                   Int -> Maybe Int -> Maybe Double -> HIplImage a c d -> 
                   HIplImage FreshImage c d
smoothGaussian' w h sigma src = 
    unsafePerformIO $
    withHIplImage src $ \src' ->
      return . fst . withCompatibleImage src $ \dst ->
        smooth src' dst cvGaussian w h' sigma' 0
    where sigma' = case sigma of { Nothing -> 0; Just s -> s }
          h' = case h of { Nothing -> 0; Just jh -> jh }

unsafeGaussian  :: (ByteOrFloat d, HasDepth d, Storable d, HasChannels c) => 
                   Int -> Maybe Int -> Maybe Double ->
                   HIplImage FreshImage c d -> HIplImage FreshImage c d
unsafeGaussian w h sigma src = unsafePerformIO $ 
                               withHIplImage src $ \src' ->
                                 do smooth src' src' cvGaussian w h' sigma' 0
                                    return src
    where sigma' = case sigma of
                     Nothing -> 0
                     Just s -> realToFrac s
          h' = case h of { Nothing -> 0; Just jh -> jh }

{-# RULES "smoothGaussian'/in-place"
    forall w h sigma (g::a->HIplImage FreshImage c d). 
    smoothGaussian' w h sigma . g = unsafeGaussian w h sigma . g 
  #-}

{-# RULES "smoothGaussian/in-place"
    forall w (g::a->HIplImage FreshImage c d).
    smoothGaussian w . g = unsafeGaussian w Nothing Nothing . g
  #-}
