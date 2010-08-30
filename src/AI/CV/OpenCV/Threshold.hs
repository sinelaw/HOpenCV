{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, 
             MultiParamTypeClasses #-}
-- |Image thresholding operations. These operations will perform
-- destructive, in-place updates when composed with a producer of
-- fresh images.
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
instance SameOrByte Float Float where
instance SameOrByte Word8 Word8 where

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
                     (HIplImage MonoChromatic d1, d1)
unsafeCvThreshold threshold maxValue tType src = 
    unsafePerformIO $
    withHIplImage src $ \srcPtr ->
      do r <- c_cvThreshold (castPtr srcPtr) (castPtr srcPtr) 
                             threshold' maxValue' tType'
         return (src, fromDouble (realToFrac r))
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType

unsafeCvThreshold1 :: ByteOrFloat d1 =>
                      d1 -> d1 -> Int -> HIplImage MonoChromatic d1 ->
                      HIplImage MonoChromatic d1
unsafeCvThreshold1 th mv tt = fst . unsafeCvThreshold th mv tt

{-# RULES 
"cvThreshold1/in-place" forall t mv tt.
  cvThreshold1 t mv tt = pipeline (unsafeCvThreshold1 t mv tt)
"cvThreshold/in-place" forall t mv tt.
  cvThreshold t mv tt = pipeline (unsafeCvThreshold t mv tt)
  #-}

-- Use Otsu's method to determine an optimal threshold value which is
-- returned along with the thresholded image.
cvThresholdOtsu :: Word8 -> Int -> HIplImage MonoChromatic Word8 ->
                   (HIplImage MonoChromatic Word8, Word8)
cvThresholdOtsu maxValue tType = cvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu

unsafeCvThresholdOtsu :: Word8 -> Int -> 
                         HIplImage MonoChromatic Word8 ->
                         (HIplImage MonoChromatic Word8, Word8)
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
                   HIplImage MonoChromatic d
unsafeThreshBin th maxValue = unsafeCvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinary

unsafeThreshBinInv :: ByteOrFloat d =>
                      d -> d -> HIplImage MonoChromatic d ->
                      HIplImage MonoChromatic d
unsafeThreshBinInv th maxValue = unsafeCvThreshold1 th maxValue tType
    where tType = fromEnum ThreshBinaryInv

{-# RULES 
"thresholdBinary/in-place" forall th mv. 
  thresholdBinary th mv = pipeline (unsafeThreshBin th mv)
"thresholdBinaryInv/in-place" forall th mv. 
  thresholdBinaryInv th mv = pipeline (unsafeThreshBinInv th mv)
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
                     HIplImage MonoChromatic d1
unsafeThreshTrunc th = unsafeCvThreshold1 th 0 (fromEnum ThreshTrunc)

{-# RULES "thresholdTruncate/in-place" forall th. 
  thresholdTruncate th = pipeline (unsafeThreshTrunc th)
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
                         HIplImage MonoChromatic d
unsafeThresholdToZero th = unsafeCvThreshold1 th 0 tType
    where tType = fromEnum ThreshToZero

unsafeThresholdToZeroInv :: ByteOrFloat d => 
                            d -> HIplImage MonoChromatic d ->
                            HIplImage MonoChromatic d
unsafeThresholdToZeroInv th = unsafeCvThreshold1 th 0 tType
    where tType = fromEnum ThreshToZeroInv


{-# RULES "thresholdToZero/in-place" forall th. 
  thresholdToZero th = pipeline (unsafeThresholdToZero th)
  #-}

{-# RULES "thresholdToZeroInv/in-place" forall th. 
  thresholdToZeroInv th = pipeline (unsafeThresholdToZeroInv th)
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
                 (HIplImage MonoChromatic Word8, Word8)
unsafeBinOtsu maxValue = unsafeCvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinary

unsafeBinOtsuInv :: Word8 -> HIplImage MonoChromatic Word8 ->
                    (HIplImage MonoChromatic Word8, Word8)
unsafeBinOtsuInv maxValue = unsafeCvThresholdOtsu maxValue tType
    where tType = fromEnum ThreshBinaryInv


{-# RULES 
"thresholdBinaryOtsu/in-place" forall mv. 
  thresholdBinaryOtsu mv = pipeline (unsafeBinOtsu mv)
"thresholdBinaryOtsuInv/in-place" forall mv. 
  thresholdBinaryOtsuInv mv = pipeline (unsafeBinOtsuInv mv)
  #-}


-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'HIplImage'; the @threshold@ value is chosen using Otsu's method
-- and returned along with the thresholded image.
thresholdTruncateOtsu :: HIplImage MonoChromatic Word8 -> 
                         (HIplImage MonoChromatic Word8, Word8)
thresholdTruncateOtsu = cvThresholdOtsu 0 (fromEnum ThreshTrunc)

unsafeTruncOtsu :: HIplImage MonoChromatic Word8 -> 
                   (HIplImage MonoChromatic Word8, Word8)
unsafeTruncOtsu = unsafeCvThresholdOtsu 0 (fromEnum ThreshTrunc)

{-# RULES "thresholdTruncateOtsu/in-place" 
  thresholdTruncateOtsu = pipeline unsafeTruncOtsu
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
                    (HIplImage MonoChromatic Word8, Word8)
unsafeToZeroOtsu = unsafeCvThresholdOtsu 0 tType
    where tType = fromEnum ThreshToZero

unsafeToZeroOtsuInv :: HIplImage MonoChromatic Word8 -> 
                       (HIplImage MonoChromatic Word8, Word8)
unsafeToZeroOtsuInv = unsafeCvThresholdOtsu 0 tType
    where tType = fromEnum ThreshToZeroInv

{-# RULES 
"thresholdToZeroOtsu/in-place" 
  thresholdToZeroOtsu = pipeline unsafeToZeroOtsu
"thresholdToZeroOtsuInv/in-place"
  thresholdToZeroOtsuInv = pipeline unsafeToZeroOtsuInv
  #-}
