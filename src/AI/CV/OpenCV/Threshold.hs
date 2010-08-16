{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
-- |Image thresholding operations. These operations will perform
-- destructive, in-place updates when composed with a producer of
-- fresh images.
module AI.CV.OpenCV.Threshold (thresholdBinary, thresholdTruncate, 
                               thresholdToZero, thresholdBinaryOtsu, 
                               thresholdTruncateOtsu, thresholdToZeroOtsu) where
import Control.Arrow (second)
import Data.Bits ((.|.))
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.CxCore 
import AI.CV.OpenCV.HIplUtils

data ThresholdType = ThreshBinary
                   | ThreshBinaryInv
                   | ThreshTrunc
                   | ThreshToZero
                   | ThreshToZeroInv
                     deriving Enum

foreign import ccall unsafe "opencv/cv.h cvThreshold"
  c_cvThreshold :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> 
                   IO (CDouble)

-- The worker function that calls c_cvThreshold.
cvThreshold :: (ByteOrFloat d, HasDepth d, Storable d) =>
               d -> d -> Int -> HIplImage a MonoChromatic d ->
               (HIplImage FreshImage MonoChromatic d, d)
cvThreshold threshold maxValue tType src = 
    unsafePerformIO $
    withHIplImage src $ \srcPtr ->
      cvtResult . withCompatibleImage src $ \dstPtr -> 
                    c_cvThreshold (castPtr srcPtr) (castPtr dstPtr) 
                                  threshold' maxValue' tType'
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType
          cvtResult = return . second (fromDouble . realToFrac)

cvThreshold1 :: (ByteOrFloat d, HasDepth d, Storable d) =>
               d -> d -> Int -> HIplImage a MonoChromatic d ->
               HIplImage FreshImage MonoChromatic d
cvThreshold1 threshold maxValue tType src = 
    fst $ cvThreshold threshold maxValue tType src

unsafeCvThreshold :: (ByteOrFloat d, HasDepth d, Storable d) =>
                      d -> d -> Int -> HIplImage FreshImage MonoChromatic d ->
                      (HIplImage FreshImage MonoChromatic d, d)
unsafeCvThreshold threshold maxValue tType src = 
    unsafePerformIO $
    withHIplImage src $ \srcPtr ->
      do r <- c_cvThreshold (castPtr srcPtr) (castPtr srcPtr) 
                             threshold' maxValue' tType'
         return (src, fromDouble (realToFrac r))
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType

unsafeCvThreshold1 :: (ByteOrFloat d, HasDepth d, Storable d) =>
                      d -> d -> Int -> HIplImage FreshImage MonoChromatic d ->
                      HIplImage FreshImage MonoChromatic d
unsafeCvThreshold1 th mv tt = fst . unsafeCvThreshold th mv tt

-- Use Otsu's method to determine an optimal threshold value which is
-- returned along with the thresholded image.
cvThresholdOtsu :: Word8 -> Int -> HIplImage a MonoChromatic Word8 ->
                   (HIplImage FreshImage MonoChromatic Word8, Word8)
cvThresholdOtsu maxValue tType = cvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu

unsafeCvThresholdOtsu :: Word8 -> Int -> 
                         HIplImage FreshImage MonoChromatic Word8 ->
                         (HIplImage FreshImage MonoChromatic Word8, Word8)
unsafeCvThresholdOtsu maxValue tType = unsafeCvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu

-- |Binary thresholding. Each pixel is mapped to zero or
-- @maxValue@. If @inverse@ is 'False', then pixels whose value is
-- greater than @threshold@ are mapped to @maxValue@; if @inverse@ is
-- 'True', then pixels whose value is less than or equal to
-- @threshold@ are mapped to @maxValue@.  Takes the source
-- 'HIplImage', the @threshold@ value, the @maxValue@ passing pixels
-- are mapped to, and the @inverse@ flag.
thresholdBinary :: (ByteOrFloat d, HasDepth d, Storable d) =>
                   d -> d -> Bool -> HIplImage a MonoChromatic d ->
                   HIplImage FreshImage MonoChromatic d
thresholdBinary th maxValue inverse = cvThreshold1 th maxValue tType
    where tType = fromEnum $ if inverse then ThreshBinaryInv else ThreshBinary

unsafeThreshBin :: (ByteOrFloat d, HasDepth d, Storable d) =>
                   d -> d -> Bool -> HIplImage FreshImage MonoChromatic d ->
                   HIplImage FreshImage MonoChromatic d
unsafeThreshBin th maxValue inverse = unsafeCvThreshold1 th maxValue tType
    where tType = fromEnum $ if inverse then ThreshBinaryInv else ThreshBinary

{-# RULES "thresholdBinary-inplace"
  forall th mv f (g::a -> HIplImage FreshImage MonoChromatic d).
  thresholdBinary th mv f . g = unsafeThreshBin th mv f . g
  #-}

-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'HIplImage' and the @threshold@ value.
thresholdTruncate :: (ByteOrFloat d, HasDepth d, Storable d, Num d) => 
                     d -> HIplImage a MonoChromatic d ->
                     HIplImage FreshImage MonoChromatic d
thresholdTruncate threshold = cvThreshold1 threshold 0 (fromEnum ThreshTrunc)

unsafeThreshTrunc :: (ByteOrFloat d, HasDepth d, Storable d, Num d) => 
                     d -> HIplImage FreshImage MonoChromatic d ->
                     HIplImage FreshImage MonoChromatic d
unsafeThreshTrunc th = unsafeCvThreshold1 th 0 (fromEnum ThreshTrunc)

{-# RULES "thresholdTruncate-inplace"
  forall th (g::a -> HIplImage FreshImage MonoChromatic d).
  thresholdTruncate th . g = unsafeThreshTrunc th . g
  #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchaged. If @inverse@ is 'True', the
-- operation's meaning is reversed. Takes the source 'HIplImage', the
-- @threshold@ value, and the @inverse@ flag.
thresholdToZero :: (ByteOrFloat d, HasDepth d, Storable d, Num d) => 
                   d -> Bool -> HIplImage a MonoChromatic d ->
                   HIplImage FreshImage MonoChromatic d
thresholdToZero threshold inverse = cvThreshold1 threshold 0 tType
    where tType = fromEnum $ if inverse then ThreshToZeroInv else ThreshToZero

unsafeThresholdToZero :: (ByteOrFloat d, HasDepth d, Storable d, Num d) => 
                         d -> Bool -> HIplImage FreshImage MonoChromatic d ->
                         HIplImage FreshImage MonoChromatic d
unsafeThresholdToZero th inv = unsafeCvThreshold1 th 0 tType
    where tType = fromEnum $ if inv then ThreshToZeroInv else ThreshToZero

{-# RULES "thresholdToZero-inplace" 
  forall th f (g::a -> HIplImage FreshImage MonoChromatic d).
  thresholdToZero th f . g = unsafeThresholdToZero th f . g
  #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the source 'HIplImage' and the @maxValue@
-- to replace pixels that pass the threshold with.
thresholdBinaryOtsu :: Word8 -> Bool -> HIplImage a MonoChromatic Word8 ->
                       (HIplImage FreshImage MonoChromatic Word8, Word8)
thresholdBinaryOtsu maxValue inverse = cvThresholdOtsu maxValue tType
    where tType = fromEnum $ if inverse then ThreshBinaryInv else ThreshBinary

unsafeBinOtsu :: Word8 -> Bool -> HIplImage FreshImage MonoChromatic Word8 ->
                 (HIplImage FreshImage MonoChromatic Word8, Word8)
unsafeBinOtsu maxValue f = unsafeCvThresholdOtsu maxValue tType
    where tType = fromEnum $ if f then ThreshBinaryInv else ThreshBinary

{-# RULES "thresholdBinaryOtsu-inplace"
  forall mv f (g::a -> HIplImage FreshImage MonoChromatic Word8).
  thresholdBinaryOtsu mv f . g = unsafeBinOtsu mv f . g
  #-}

-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'HIplImage'; the @threshold@ value is chosen using Otsu's method
-- and returned along with the thresholded image.
thresholdTruncateOtsu :: HIplImage a MonoChromatic Word8 -> 
                         (HIplImage FreshImage MonoChromatic Word8, Word8)
thresholdTruncateOtsu = cvThresholdOtsu 0 (fromEnum ThreshTrunc)

unsafeTruncOtsu :: HIplImage FreshImage MonoChromatic Word8 -> 
                   (HIplImage FreshImage MonoChromatic Word8, Word8)
unsafeTruncOtsu = unsafeCvThresholdOtsu 0 (fromEnum ThreshTrunc)

{-# RULES "thresholdTruncateOtsu-inplace"
  forall (g :: a -> HIplImage FreshImage MonoChromatic Word8).
  thresholdTruncateOtsu . g = unsafeTruncOtsu . g
  #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchaged. If @inverse@ is 'True', the
-- operation's meaning is reversed. Takes the source 'HIplImage' and
-- the @inverse@ flag; the @threshold@ value is chosen using Otsu's
-- method and returned along with the thresholded image.
thresholdToZeroOtsu :: Bool -> HIplImage a MonoChromatic Word8 -> 
                       (HIplImage FreshImage MonoChromatic Word8, Word8)
thresholdToZeroOtsu inverse = cvThresholdOtsu 0 tType
    where tType = fromEnum $ if inverse then ThreshToZeroInv else ThreshToZero

unsafeToZeroOtsu :: Bool -> HIplImage FreshImage MonoChromatic Word8 -> 
                    (HIplImage FreshImage MonoChromatic Word8, Word8)
unsafeToZeroOtsu f = unsafeCvThresholdOtsu 0 tType
    where tType = fromEnum $ if f then ThreshToZeroInv else ThreshToZero

{-# RULES "thresholdToZeroOtsu-inplace"
  forall f (g :: a -> HIplImage FreshImage MonoChromatic Word8).
  thresholdToZeroOtsu f . g = unsafeToZeroOtsu f . g
  #-}