{-# LANGUAGE ForeignFunctionInterface #-}
-- |Image filtering operations.
module AI.CV.OpenCV.Filtering (smoothGaussian, smoothGaussian') where
import Foreign.C.Types (CInt, CDouble)
import Foreign.Ptr (Ptr, castPtr)
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp

#include <opencv2/imgproc/types_c.h>

foreign import ccall "opencv2/imgproc/imgproc_c.h cvSmooth"
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
smoothGaussian :: (ByteOrFloat d, HasChannels c, Inplace r c d c d) => 
                  Int -> HIplImage c d r -> HIplImage c d r
smoothGaussian w = smoothGaussian' w Nothing Nothing
{-# INLINE smoothGaussian #-}

-- |Smooth a source 'HIplImage' using a linear convolution with a
-- Gaussian kernel. Parameters are the kernel width, the kernel height
-- (if 'Nothing', the height will be set to the same value as the
-- width), the Gaussian standard deviation (if 'Nothing', it will be
-- calculated from the kernel size), and the source image. May be
-- performed in-place under composition.
smoothGaussian' :: (ByteOrFloat d, HasChannels c, Inplace r c d c d) => 
                   Int -> Maybe Int -> Maybe Double -> HIplImage c d r -> 
                   HIplImage c d r
smoothGaussian' w h sigma = 
    cv2 $ \src dst -> smooth src dst cvGaussian w h' sigma' 0
    where sigma' = case sigma of { Nothing -> 0; Just s -> s }
          h' = case h of { Nothing -> 0; Just jh -> jh }
{-# INLINE smoothGaussian' #-}
