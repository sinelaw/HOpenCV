{-# LANGUAGE ForeignFunctionInterface, TypeFamilies #-}
-- |Image filtering operations.
module OpenCV.Filtering (smoothGaussian, smoothGaussian', 
                         sobel, sobelDX, sobelDY,
                         ApertureSize(..), DerivativeOrder(..)) where
import Data.Word (Word8)
import Data.Int (Int16)
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr, castPtr)
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp

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
-- 'Image'. The kernel height will be set to the same value as the
-- width, and the Gaussian standard deviation will be calculated from
-- the kernel size. This function is the same as calling
-- @smoothGaussian' width Nothing Nothing@. May be performed in-place
-- under composition.
smoothGaussian :: (ByteOrFloat d, Inplace r c d c d) => 
                  Int -> Image c d r -> Image c d r
smoothGaussian w = smoothGaussian' w Nothing Nothing
{-# INLINE smoothGaussian #-}

-- |Smooth a source 'Image' using a linear convolution with a
-- Gaussian kernel. Parameters are the kernel width, the kernel height
-- (if 'Nothing', the height will be set to the same value as the
-- width), the Gaussian standard deviation (if 'Nothing', it will be
-- calculated from the kernel size), and the source image. May be
-- performed in-place under composition.
smoothGaussian' :: (ByteOrFloat d, Inplace r c d c d) => 
                   Int -> Maybe Int -> Maybe Double -> Image c d r -> 
                   Image c d r
smoothGaussian' w h sigma = 
    cv2 $ \src dst -> smooth src dst cvGaussian w h' sigma' 0
    where sigma' = case sigma of { Nothing -> 0; Just s -> s }
          h' = case h of { Nothing -> 0; Just jh -> jh }
{-# INLINE smoothGaussian' #-}

foreign import ccall "opencv2/imgproc/imgproc_c.h cvSobel"
  cvSobel :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> CInt -> IO ()

-- |Size of the extended Sobel kernel. When 'ApertureOne' is used, a
-- 3x1 or 1x3 kernel is used with no Gaussian smoothing. Note that
-- 'ApertureOne' can only be used for the first or second x or y
-- derivatives.
data ApertureSize = ApertureOne | ApertureThree | ApertureFive | ApertureSeven

-- |Order of the derivative computed using a Sobel operator.
data DerivativeOrder = OrderZero | OrderOne | OrderTwo | OrderThree

apertureToInt :: ApertureSize -> CInt
apertureToInt ApertureOne   = 1
apertureToInt ApertureThree = 3
apertureToInt ApertureFive  = 5
apertureToInt ApertureSeven = 7

orderToInt :: DerivativeOrder -> CInt
orderToInt OrderZero  = 0
orderToInt OrderOne   = 1
orderToInt OrderTwo   = 2
orderToInt OrderThree = 3

type family SobelDest a :: *
type instance SobelDest Word8 = Int16
type instance SobelDest Float = Float

-- |Calculates the first, second, third or mixed image derivatives
-- using an extended Sobel operators. @sobel xOrder yOrder apertureSize img@
sobel :: (HasDepth d1, HasDepth d2, d2 ~ SobelDest d1, Inplace r c d1 c d2) =>
         DerivativeOrder -> DerivativeOrder -> ApertureSize -> 
         Image c d1 r -> Image c d2 r
sobel xOrder yOrder apertureSize = cv2 $ \src dst -> cvSobel src dst x y ap
  where ap = apertureToInt apertureSize
        x = orderToInt xOrder
        y = orderToInt yOrder
{-# INLINE sobel #-}

-- |Compute the first X derivative of an image using a Sobel operator.
sobelDX :: (HasDepth d1, HasDepth d2, d2 ~ SobelDest d1, Inplace r c d1 c d2) =>
           ApertureSize -> Image c d1 r -> Image c d2 r
sobelDX = sobel OrderOne OrderZero
{-# INLINE sobelDX #-}

-- |Compute the first Y derivative of an image using a Sobel operator.
sobelDY :: (HasDepth d1, HasDepth d2, d2 ~ SobelDest d1, Inplace r c d1 c d2) =>
            ApertureSize -> Image c d1 r -> Image c d2 r
sobelDY = sobel OrderZero OrderOne
{-# INLINE sobelDY #-}