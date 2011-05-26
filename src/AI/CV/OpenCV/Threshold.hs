{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, 
             MultiParamTypeClasses, FlexibleInstances #-}
-- |Image thresholding operations. These operations will perform
-- destructive, in-place updates when used in compositions.
module AI.CV.OpenCV.Threshold (thresholdBinary, thresholdBinaryInv,
                               thresholdTruncate, 
                               thresholdToZero, thresholdToZeroInv,
                               thresholdBinaryOtsu, thresholdBinaryOtsuInv,
                               thresholdTruncateOtsu,
                               thresholdToZeroOtsu, thresholdToZeroOtsuInv) where
import Data.Bits ((.|.))
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, castPtr)
import AI.CV.OpenCV.Core.CxCore 
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp

data ThresholdType = ThreshBinary
                   | ThreshBinaryInv
                   | ThreshTrunc
                   | ThreshToZero
                   | ThreshToZeroInv
                     deriving Enum

-- The OpenCV thresholding functions have the property that the source
-- image must be a single-channel with 8-bit or float pixels. The
-- destination image must be either the same pixel type as the source,
-- or 8-bit. This means that images of float pixels can be converted
-- to 8-bit images during the thresholding process.
class (HasDepth d1, HasDepth d2) => SameOrByte d1 d2 where
instance SameOrByte Float Word8 where
instance ByteOrFloat d => SameOrByte d d where

foreign import ccall "opencv2/imgproc/imgproc_c.h cvThreshold"
  c_cvThreshold :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> 
                   IO (CDouble)

-- The worker function that calls c_cvThreshold.
cvThreshold :: (ByteOrFloat d1, SameOrByte d1 d2) =>
               d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
               HIplImage MonoChromatic d2
cvThreshold threshold maxValue tType = 
    cv2 $ \src dst ->
        do _r <- c_cvThreshold (castPtr src) (castPtr dst) 
                               threshold' maxValue' tType'
           return ()
           --return (fromDouble (realToFrac r)) -- FIXME: This is dropped by cv2!
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType
{-# INLINE cvThreshold #-}

cvThreshold1 :: (ByteOrFloat d1, SameOrByte d1 d2) =>
               d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
               HIplImage MonoChromatic d2
cvThreshold1 threshold maxValue tType = 
    cvThreshold threshold maxValue tType
{-# INLINE cvThreshold1 #-}

-- Use Otsu's method to determine an optimal threshold value which is
-- returned along with the thresholded image.
cvThresholdOtsu :: Word8 -> Int -> HIplImage MonoChromatic Word8 ->
                   HIplImage MonoChromatic Word8
cvThresholdOtsu maxValue tType = cvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu
{-# INLINE cvThresholdOtsu #-}

-- |Binary thresholding. Parameters are the @threshold@ value, the
-- @maxValue@ passing pixels are mapped to, and the source
-- 'HIplImage'. Each pixel greater than @threshold@ is mapped to
-- @maxValue@, while all others are mapped to zero.
thresholdBinary :: (ByteOrFloat d1, SameOrByte d1 d2) =>
                   d1 -> d1 -> HIplImage MonoChromatic d1 ->
                   HIplImage MonoChromatic d2
thresholdBinary th maxValue = cvThreshold1 th maxValue (fromEnum ThreshBinary)
{-# INLINE thresholdBinary #-}

-- |Inverse binary thresholding. Parameters are the @threshold@ value,
-- the @maxValue@ passing pixels are mapped to, and the source
-- 'HIplImage'. Each pixel greater than @threshold@ is mapped to zero,
-- while all others are mapped to @maxValue@.
thresholdBinaryInv :: (ByteOrFloat d1, SameOrByte d1 d2) =>
                      d1 -> d1 -> HIplImage MonoChromatic d1 ->
                      HIplImage MonoChromatic d2
thresholdBinaryInv th maxValue = cvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinaryInv
{-# INLINE thresholdBinaryInv #-}

-- |Truncation thresholding (i.e. clamping). Parameters are the
-- @threshold@ value and the source 'HIplImage'. Maps pixels that are
-- greater than @threshold@ to the @threshold@ value; leaves all other
-- pixels unchanged.
thresholdTruncate :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                     d1 -> HIplImage MonoChromatic d1 ->
                     HIplImage MonoChromatic d2
thresholdTruncate threshold = cvThreshold1 threshold 0 (fromEnum ThreshTrunc)
{-# INLINE thresholdTruncate #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchanged. Parameters the @threshold@ value
-- and the source 'HIplImage'.
thresholdToZero :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                   d1 -> HIplImage MonoChromatic d1 ->
                   HIplImage MonoChromatic d2
thresholdToZero threshold = cvThreshold1 threshold 0 (fromEnum ThreshToZero)
{-# INLINE thresholdToZero #-}

-- |Maps pixels that are greater than @threshold@ to zero; leaves all
-- other pixels unchanged. Parameters the @threshold@ value and the
-- source 'HIplImage'.
thresholdToZeroInv :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                      d1 -> HIplImage MonoChromatic d1 ->
                      HIplImage MonoChromatic d2
thresholdToZeroInv threshold = cvThreshold1 threshold 0 tType
    where tType = fromEnum ThreshToZeroInv
{-# INLINE thresholdToZeroInv #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ to replace pixels that pass
-- the threshold with and the source 'HIplImage'.
thresholdBinaryOtsu :: Word8 -> HIplImage MonoChromatic Word8 ->
                       HIplImage MonoChromatic Word8
thresholdBinaryOtsu maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinary
{-# INLINE thresholdBinaryOtsu #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ to replace pixels that pass
-- the threshold with and the source 'HIplImage'. The sense of the
-- thresholding operation is inverted, as in 'thresholdBinaryInv'.
thresholdBinaryOtsuInv :: Word8 -> HIplImage MonoChromatic Word8 ->
                          HIplImage MonoChromatic Word8
thresholdBinaryOtsuInv maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinaryInv
{-# INLINE thresholdBinaryOtsuInv #-}

-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'HIplImage'; the @threshold@ value is chosen using Otsu's method
-- and returned along with the thresholded image.
thresholdTruncateOtsu :: HIplImage MonoChromatic Word8 -> 
                         HIplImage MonoChromatic Word8
thresholdTruncateOtsu = cvThresholdOtsu 0 (fromEnum ThreshTrunc)
{-# INLINE thresholdTruncateOtsu #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchaged.The @threshold@ value is chosen
-- using Otsu's method and returned along with the thresholded image.
thresholdToZeroOtsu :: HIplImage MonoChromatic Word8 -> 
                       HIplImage MonoChromatic Word8
thresholdToZeroOtsu = cvThresholdOtsu 0 (fromEnum ThreshToZero)
{-# INLINE thresholdToZeroOtsu #-}

-- |Maps pixels that are greather than @threshold@ to zero; leaves all
-- other pixels unchaged.The @threshold@ value is chosen using Otsu's
-- method and returned along with the thresholded image.
thresholdToZeroOtsuInv :: HIplImage MonoChromatic Word8 -> 
                          HIplImage MonoChromatic Word8
thresholdToZeroOtsuInv = cvThresholdOtsu 0 (fromEnum ThreshToZeroInv)
{-# INLINE thresholdToZeroOtsuInv #-}
