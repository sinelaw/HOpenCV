{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, 
             MultiParamTypeClasses, FlexibleInstances #-}
-- |Image thresholding operations. These operations will perform
-- destructive, in-place updates when composed with a producer of
-- fresh images.
module AI.CV.OpenCV.Threshold (thresholdBinary, thresholdBinaryInv,
                               thresholdTruncate, 
                               thresholdToZero, thresholdToZeroInv,
                               thresholdBinaryOtsu, thresholdBinaryOtsuInv,
                               thresholdTruncateOtsu, unsafeThreshBin,
                               thresholdToZeroOtsu, thresholdToZeroOtsuInv) where
import Data.Bits ((.|.))
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore 
import AI.CV.OpenCV.Core.HIplUtils

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

foreign import ccall unsafe "opencv/cv.h cvThreshold"
  c_cvThreshold :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> 
                   IO (CDouble)

-- The worker function that calls c_cvThreshold.
cvThreshold :: (ByteOrFloat d1, SameOrByte d1 d2) =>
               d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
               (HIplImage MonoChromatic d2, d1)
cvThreshold threshold maxValue tType src = 
    unsafePerformIO $
    withHIplImage src $ \srcPtr ->
        do dst <- mkHIplImage (width src) (height src)
           r <- withHIplImage dst $ \dstPtr ->
                  c_cvThreshold (castPtr srcPtr) (castPtr dstPtr) 
                                threshold' maxValue' tType'
           return (dst, fromDouble (realToFrac r))
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType

cvThreshold1 :: (ByteOrFloat d1, SameOrByte d1 d2) =>
               d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
               HIplImage MonoChromatic d2
cvThreshold1 threshold maxValue tType src = 
    fst $ cvThreshold threshold maxValue tType src

unsafeCvThreshold :: ByteOrFloat d1 =>
                     d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
                     IO (HIplImage MonoChromatic d1, d1)
unsafeCvThreshold threshold maxValue tType src = 
    withHIplImage src $ \srcPtr ->
      do r <- c_cvThreshold (castPtr srcPtr) (castPtr srcPtr) 
                             threshold' maxValue' tType'
         return (src, fromDouble (realToFrac r))
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType

unsafeCvThreshold1 :: ByteOrFloat d1 =>
                      d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
                      IO (HIplImage MonoChromatic d1)
unsafeCvThreshold1 th mv tt = fmap fst . unsafeCvThreshold th mv tt

-- Use Otsu's method to determine an optimal threshold value which is
-- returned along with the thresholded image.
cvThresholdOtsu :: Word8 -> Int -> HIplImage MonoChromatic Word8 ->
                   (HIplImage MonoChromatic Word8, Word8)
cvThresholdOtsu maxValue tType = cvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu

unsafeCvThresholdOtsu :: Word8 -> Int -> 
                         HIplImage MonoChromatic Word8 ->
                         IO (HIplImage MonoChromatic Word8, Word8)
unsafeCvThresholdOtsu maxValue tType = unsafeCvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu

-- |Binary thresholding. Parameters are the @threshold@ value, the
-- @maxValue@ passing pixels are mapped to, and the source
-- 'HIplImage'. Each pixel greater than @threshold@ is mapped to
-- @maxValue@, while all others are mapped to zero.
thresholdBinary :: (ByteOrFloat d1, SameOrByte d1 d2) =>
                   d1 -> d1 -> HIplImage MonoChromatic d1 ->
                   HIplImage MonoChromatic d2
thresholdBinary th maxValue = cvThreshold1 th maxValue (fromEnum ThreshBinary)
{-# INLINE [1] thresholdBinary #-}

-- |Inverse binary thresholding. Parameters are the @threshold@ value,
-- the @maxValue@ passing pixels are mapped to, and the source
-- 'HIplImage'. Each pixel greater than @threshold@ is mapped to zero,
-- while all others are mapped to @maxValue@.
thresholdBinaryInv :: (ByteOrFloat d1, SameOrByte d1 d2) =>
                      d1 -> d1 -> HIplImage MonoChromatic d1 ->
                      HIplImage MonoChromatic d2
thresholdBinaryInv th maxValue = cvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinaryInv

unsafeThreshBin :: ByteOrFloat d =>
                   d -> d -> HIplImage MonoChromatic d ->
                   IO (HIplImage MonoChromatic d)
unsafeThreshBin th maxValue = unsafeCvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinary
{-# INLINE [1] unsafeThreshBin #-}

unsafeThreshBinInv :: ByteOrFloat d =>
                      d -> d -> HIplImage MonoChromatic d ->
                      IO (HIplImage MonoChromatic d)
unsafeThreshBinInv th maxValue = unsafeCvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinaryInv

{-# RULES 
"thresholdBinary/in-place" [~1] forall th mv.
  thresholdBinary th mv = pipeline (unsafeThreshBin th mv)
"thresholdBinary/unpipe" [1] forall th mv.
  pipeline (unsafeThreshBin th mv) = thresholdBinary th mv
"thresholdBinaryInv/in-place" [~1] forall th mv. 
  thresholdBinaryInv th mv = pipeline (unsafeThreshBinInv th mv)
"thresholdBinaryInv/unpipe" [1] forall th mv.
  pipeline (unsafeThreshBinInv th mv) = thresholdBinaryInv th mv
  #-}

-- |Truncation thresholding (i.e. clamping). Parameters are the
-- @threshold@ value and the source 'HIplImage'. Maps pixels that are
-- greater than @threshold@ to the @threshold@ value; leaves all other
-- pixels unchanged.
thresholdTruncate :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                     d1 -> HIplImage MonoChromatic d1 ->
                     HIplImage MonoChromatic d2
thresholdTruncate threshold = cvThreshold1 threshold 0 (fromEnum ThreshTrunc)

unsafeThreshTrunc :: ByteOrFloat d1 =>
                     d1 -> HIplImage MonoChromatic d1 ->
                     IO (HIplImage MonoChromatic d1)
unsafeThreshTrunc th = unsafeCvThreshold1 th 0 (fromEnum ThreshTrunc)

{-# INLINE [0] thresholdTruncate #-}
{-# INLINE [0] unsafeThreshTrunc #-}
{-# RULES 
"thresholdTruncate/in-place" [~1] forall th. 
  thresholdTruncate th = pipeline (unsafeThreshTrunc th)
"thresholdTruncate/unpipe" [1] forall th.
  pipeline (unsafeThreshTrunc th) = thresholdTruncate th
  #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchanged. Parameters the @threshold@ value
-- and the source 'HIplImage'.
thresholdToZero :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                   d1 -> HIplImage MonoChromatic d1 ->
                   HIplImage MonoChromatic d2
thresholdToZero threshold = cvThreshold1 threshold 0 (fromEnum ThreshToZero)

-- |Maps pixels that are greater than @threshold@ to zero; leaves all
-- other pixels unchanged. Parameters the @threshold@ value and the
-- source 'HIplImage'.
thresholdToZeroInv :: (ByteOrFloat d1, SameOrByte d1 d2) => 
                      d1 -> HIplImage MonoChromatic d1 ->
                      HIplImage MonoChromatic d2
thresholdToZeroInv threshold = cvThreshold1 threshold 0 tType
    where tType = fromEnum ThreshToZeroInv

unsafeThresholdToZero :: ByteOrFloat d => 
                         d -> HIplImage MonoChromatic d ->
                         IO (HIplImage MonoChromatic d)
unsafeThresholdToZero th = unsafeCvThreshold1 th 0 tType
    where tType = fromEnum ThreshToZero

unsafeThresholdToZeroInv :: ByteOrFloat d => 
                            d -> HIplImage MonoChromatic d ->
                            IO (HIplImage MonoChromatic d)
unsafeThresholdToZeroInv th = unsafeCvThreshold1 th 0 tType
    where tType = fromEnum ThreshToZeroInv

{-# INLINE [0] thresholdToZero #-}
{-# INLINE [0] unsafeThresholdToZero #-}
{-# INLINE [0] thresholdToZeroInv #-}
{-# INLINE [0] unsafeThresholdToZeroInv #-}
{-# RULES 
"thresholdToZero/in-place" [~1] forall th. 
  thresholdToZero th = pipeline (unsafeThresholdToZero th)
"thresholdToZero/unpipe" [1] forall th.
  pipeline (unsafeThresholdToZero th) = thresholdToZero th
"thresholdToZeroInv/in-place" [~1] forall th. 
  thresholdToZeroInv th = pipeline (unsafeThresholdToZeroInv th)
"thresholdToZeroInv/unpipe" [1] forall th.
  pipeline (unsafeThresholdToZeroInv th) = thresholdToZeroInv th
  #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ to replace pixels that pass
-- the threshold with and the source 'HIplImage'.
thresholdBinaryOtsu :: Word8 -> HIplImage MonoChromatic Word8 ->
                       (HIplImage MonoChromatic Word8, Word8)
thresholdBinaryOtsu maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinary

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ to replace pixels that pass
-- the threshold with and the source 'HIplImage'. The sense of the
-- thresholding operation is inverted, as in 'thresholdBinaryInv'.
thresholdBinaryOtsuInv :: Word8 -> HIplImage MonoChromatic Word8 ->
                          (HIplImage MonoChromatic Word8, Word8)
thresholdBinaryOtsuInv maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinaryInv

unsafeBinOtsu :: Word8 -> HIplImage MonoChromatic Word8 ->
                 IO (HIplImage MonoChromatic Word8, Word8)
unsafeBinOtsu maxValue = unsafeCvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinary

unsafeBinOtsuInv :: Word8 -> HIplImage MonoChromatic Word8 ->
                    IO (HIplImage MonoChromatic Word8, Word8)
unsafeBinOtsuInv maxValue = unsafeCvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinaryInv

{-# INLINE [0] thresholdBinaryOtsu #-}
{-# INLINE [0] unsafeBinOtsu #-}
{-# INLINE [0] thresholdBinaryOtsuInv #-}
{-# INLINE [0] unsafeBinOtsuInv #-}
{-# RULES 
"thresholdBinaryOtsu/in-place" [~1] forall mv. 
  thresholdBinaryOtsu mv = pipeline (unsafeBinOtsu mv)
"thresholdBinaryOtsu/unpipe" [1] forall mv.
  pipeline (unsafeBinOtsu mv) = thresholdBinaryOtsu mv
"thresholdBinaryOtsuInv/in-place" [~1] forall mv. 
  thresholdBinaryOtsuInv mv = pipeline (unsafeBinOtsuInv mv)
"thresholdBinaryOtsuInv/unpipe" [1] forall mv.
  pipeline (unsafeBinOtsuInv mv) = thresholdBinaryOtsuInv mv
  #-}

-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'HIplImage'; the @threshold@ value is chosen using Otsu's method
-- and returned along with the thresholded image.
thresholdTruncateOtsu :: HIplImage MonoChromatic Word8 -> 
                         (HIplImage MonoChromatic Word8, Word8)
thresholdTruncateOtsu = cvThresholdOtsu 0 (fromEnum ThreshTrunc)

unsafeTruncOtsu :: HIplImage MonoChromatic Word8 -> 
                   IO (HIplImage MonoChromatic Word8, Word8)
unsafeTruncOtsu = unsafeCvThresholdOtsu 0 (fromEnum ThreshTrunc)

{-# INLINE [0] thresholdTruncateOtsu #-}
{-# INLINE [0] unsafeTruncOtsu #-}
{-# RULES 
"thresholdTruncateOtsu/in-place" [~1] 
  thresholdTruncateOtsu = pipeline unsafeTruncOtsu
"thresholdTruncateOtsu/unpipe" [1]
  pipeline unsafeTruncOtsu = thresholdTruncateOtsu
  #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchaged.The @threshold@ value is chosen
-- using Otsu's method and returned along with the thresholded image.
thresholdToZeroOtsu :: HIplImage MonoChromatic Word8 -> 
                       (HIplImage MonoChromatic Word8, Word8)
thresholdToZeroOtsu = cvThresholdOtsu 0 (fromEnum ThreshToZero)

-- |Maps pixels that are greather than @threshold@ to zero; leaves all
-- other pixels unchaged.The @threshold@ value is chosen using Otsu's
-- method and returned along with the thresholded image.
thresholdToZeroOtsuInv :: HIplImage MonoChromatic Word8 -> 
                          (HIplImage MonoChromatic Word8, Word8)
thresholdToZeroOtsuInv = cvThresholdOtsu 0 (fromEnum ThreshToZeroInv)

unsafeToZeroOtsu :: HIplImage MonoChromatic Word8 -> 
                    IO (HIplImage MonoChromatic Word8, Word8)
unsafeToZeroOtsu = unsafeCvThresholdOtsu 0 tType
    where tType = fromEnum ThreshToZero

unsafeToZeroOtsuInv :: HIplImage MonoChromatic Word8 -> 
                       IO (HIplImage MonoChromatic Word8, Word8)
unsafeToZeroOtsuInv = unsafeCvThresholdOtsu 0 tType
    where tType = fromEnum ThreshToZeroInv

{-# INLINE [0] thresholdToZeroOtsu #-}
{-# INLINE [0] unsafeToZeroOtsu #-}
{-# INLINE [0] thresholdToZeroOtsuInv #-}
{-# INLINE [0] unsafeToZeroOtsuInv #-}
{-# RULES 
"thresholdToZeroOtsu/in-place" [~1]
  thresholdToZeroOtsu = pipeline unsafeToZeroOtsu
"thresholdToZeroOtsu/unpipe" [1]
  pipeline unsafeToZeroOtsu = thresholdToZeroOtsu
"thresholdToZeroOtsuInv/in-place" [~1]
  thresholdToZeroOtsuInv = pipeline unsafeToZeroOtsuInv
"thresholdToZeroOtsuInv/unpipe" [1]
  pipeline unsafeToZeroOtsuInv = thresholdToZeroOtsuInv
  #-}
