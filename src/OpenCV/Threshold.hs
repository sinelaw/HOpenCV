{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, TypeFamilies,
             MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DataKinds #-}
-- |Image thresholding operations. These operations will perform
-- destructive, in-place updates when used in compositions.
module OpenCV.Threshold (thresholdBinary, thresholdBinaryInv,
                         thresholdTruncate, 
                         thresholdToZero, thresholdToZeroInv,
                         thresholdBinaryOtsu, thresholdBinaryOtsuInv,
                         thresholdTruncateOtsu,
                         thresholdToZeroOtsu, thresholdToZeroOtsuInv) where
import Data.Bits ((.|.))
import Data.Word (Word8)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr)
import OpenCV.Core.CxCore 
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp

data ThresholdType = ThreshBinary
                   | ThreshBinaryInv
                   | ThreshTrunc
                   | ThreshToZero
                   | ThreshToZeroInv
                     deriving Enum

fromEnumC :: Enum a => a -> CInt
fromEnumC = fromIntegral . fromEnum

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
                   IO CDouble

{-
class ByteOrFloat a => Thresholdable r a b where
  doThreshold :: (ImgBuilder r, Inplace r a Monochromatic b Monochromatic) => 
                 a -> a -> Int -> 
                 HIplImage Monochromatic a r -> 
                 HIplImage Monochromatic b r

instance Inplace r Monochromatic Word8 Monochromatic Word8 =>
         Thresholdable r Word8 Word8 where
  doThreshold = cvThreshold1
  {-# INLINE doThreshold #-}

instance Inplace r Monochromatic Float Monochromatic Float =>
         Thresholdable r Float Float where
  doThreshold = cvThreshold1
  {-# INLINE doThreshold #-}

instance Inplace r Monochromatic Float Monochromatic Word8 =>
         Thresholdable r Float Word8 where
  doThreshold = cvThreshold2
  {-# INLINE doThreshold #-}

cvThreshold1 :: (ByteOrFloat a, ImgBuilder r, Inplace r M a M a) => 
                a -> a -> Int -> 
                HIplImage Monochromatic a r -> HIplImage Monochromatic a r
cvThreshold1 threshold maxValue tType =
  cv $ \src -> void $ c_cvThreshold src src threshold' maxValue' tType'
  where threshold' = realToFrac . toDouble $ threshold
        maxValue' = realToFrac . toDouble $ maxValue
        tType' = fromIntegral tType
{-# INLINE cvThreshold1 #-}
-}

-- The worker function that calls c_cvThreshold.
cvThreshold2 :: (ByteOrFloat d1, SameOrByte d1 d2, Inplace r M d1 M d2) =>
                d1 -> d1 -> CInt -> Image Monochromatic d1 r ->
                Image Monochromatic d2 r
cvThreshold2 threshold maxValue tType = 
    cv2 $ \src dst ->
        do _r <- c_cvThreshold src dst threshold' maxValue' tType'
           return ()
           --return (fromDouble (realToFrac r)) -- FIXME: This is dropped by cv2!
    where threshold' = realToFrac . toDouble $ threshold
          maxValue' = realToFrac . toDouble $ maxValue
          tType' = fromIntegral tType
{-# INLINE cvThreshold2 #-}

-- cvThreshold :: (Thresholdable d1 d2, ImgBuilder r) => d1 -> d1 -> Int -> 
--                HIplImage Monochromatic d1 r -> HIplImage Monochromatic d2 r
-- cvThreshold = doThreshold
cvThreshold :: (ByteOrFloat d1, SameOrByte d1 d2, Inplace r M d1 M d2) =>
               d1 -> d1 -> CInt -> 
               Image Monochromatic d1 r -> Image Monochromatic d2 r
cvThreshold = cvThreshold2

{-# INLINE cvThreshold #-}

-- Use Otsu's method to determine an optimal threshold value which is
-- returned along with the thresholded image.
cvThresholdOtsu :: Inplace r M Word8 M Word8 =>
                   Word8 -> CInt -> Image Monochromatic Word8 r ->
                   Image Monochromatic Word8 r
cvThresholdOtsu maxValue tType = cvThreshold 0 maxValue tType'
    where otsu = 8
          tType' = tType .|. otsu
{-# INLINE cvThresholdOtsu #-}

type M = Monochromatic

-- |Binary thresholding. Parameters are the @threshold@ value, the
-- @maxValue@ passing pixels are mapped to, and the source
-- 'Image'. Each pixel greater than @threshold@ is mapped to
-- @maxValue@, while all others are mapped to zero.
thresholdBinary :: (SameOrByte d1 d2, ByteOrFloat d1, Inplace r M d1 M d2) => 
                   d1 -> d1 -> 
                   Image Monochromatic d1 r -> Image Monochromatic d2 r
thresholdBinary th maxValue = cvThreshold th maxValue (fromEnumC ThreshBinary)
{-# INLINE thresholdBinary #-}

-- |Inverse binary thresholding. Parameters are the @threshold@ value,
-- the @maxValue@ passing pixels are mapped to, and the source
-- 'Image'. Each pixel greater than @threshold@ is mapped to zero,
-- while all others are mapped to @maxValue@.
thresholdBinaryInv :: (SameOrByte d1 d2, ByteOrFloat d1, Inplace r M d1 M d2) => 
                      d1 -> d1 -> 
                      Image Monochromatic d1 r -> Image Monochromatic d2 r
thresholdBinaryInv th maxValue = cvThreshold th maxValue tType
    where tType = fromEnumC ThreshBinaryInv
{-# INLINE thresholdBinaryInv #-}

-- |Truncation thresholding (i.e. clamping). Parameters are the
-- @threshold@ value and the source 'Image'. Maps pixels that are
-- greater than @threshold@ to the @threshold@ value; leaves all other
-- pixels unchanged.
thresholdTruncate :: (SameOrByte d1 d2, ByteOrFloat d1, Inplace r M d1 M d2) => 
                     d1 -> Image Monochromatic d1 r -> Image Monochromatic d2 r
thresholdTruncate threshold = cvThreshold threshold 0 (fromEnumC ThreshTrunc)
{-# INLINE thresholdTruncate #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchanged. Parameters the @threshold@ value
-- and the source 'Image'.
thresholdToZero :: (SameOrByte d1 d2, ByteOrFloat d1, Inplace r M d1 M d2) => 
                   d1 -> Image Monochromatic d1 r -> Image Monochromatic d2 r
thresholdToZero threshold = cvThreshold threshold 0 (fromEnumC ThreshToZero)
{-# INLINE thresholdToZero #-}

-- |Maps pixels that are greater than @threshold@ to zero; leaves all
-- other pixels unchanged. Parameters the @threshold@ value and the
-- source 'Image'.
thresholdToZeroInv :: (SameOrByte d1 d2, ByteOrFloat d1, Inplace r M d1 M d2) => 
                      d1 -> Image Monochromatic d1 r -> Image Monochromatic d2 r
thresholdToZeroInv threshold = cvThreshold threshold 0 tType
    where tType = fromEnumC ThreshToZeroInv
{-# INLINE thresholdToZeroInv #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ used to replace pixels that
-- pass the threshold with and the source 'Image'.
thresholdBinaryOtsu :: (Inplace r M Word8 M Word8) =>
                       Word8 -> Image Monochromatic Word8 r ->
                       Image Monochromatic Word8 r
thresholdBinaryOtsu maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnumC ThreshBinary
{-# INLINE thresholdBinaryOtsu #-}

-- |Binary thresholding using Otsu's method to determine an optimal
-- threshold value. The chosen value is returned along with the
-- thresholded image. Takes the @maxValue@ to replace pixels that pass
-- the threshold with and the source 'Image'. The sense of the
-- thresholding operation is inverted, as in 'thresholdBinaryInv'.
thresholdBinaryOtsuInv :: (Inplace r M Word8 M Word8) =>
                          Word8 -> Image Monochromatic Word8 r ->
                          Image Monochromatic Word8 r
thresholdBinaryOtsuInv maxValue = cvThresholdOtsu maxValue tType
    where tType = fromEnumC ThreshBinaryInv
{-# INLINE thresholdBinaryOtsuInv #-}

-- |Maps pixels that are greater than @threshold@ to the @threshold@
-- value; leaves all other pixels unchanged. Takes the source
-- 'Image'; the @threshold@ value is chosen using Otsu's method
-- and returned along with the thresholded image.
thresholdTruncateOtsu :: (Inplace r M Word8 M Word8) =>
                         Image Monochromatic Word8 r -> 
                         Image Monochromatic Word8 r
thresholdTruncateOtsu = cvThresholdOtsu 0 (fromEnumC ThreshTrunc)
{-# INLINE thresholdTruncateOtsu #-}

-- |Maps pixels that are less than or equal to @threshold@ to zero;
-- leaves all other pixels unchaged.The @threshold@ value is chosen
-- using Otsu's method and returned along with the thresholded image.
thresholdToZeroOtsu :: (Inplace r M Word8 M Word8) =>
                       Image Monochromatic Word8 r -> 
                       Image Monochromatic Word8 r
thresholdToZeroOtsu = cvThresholdOtsu 0 (fromEnumC ThreshToZero)
{-# INLINE thresholdToZeroOtsu #-}

-- |Maps pixels that are greather than @threshold@ to zero; leaves all
-- other pixels unchaged.The @threshold@ value is chosen using Otsu's
-- method and returned along with the thresholded image.
thresholdToZeroOtsuInv :: (Inplace r M Word8 M Word8) =>
                          Image Monochromatic Word8 r -> 
                          Image Monochromatic Word8 r
thresholdToZeroOtsuInv = cvThresholdOtsu 0 (fromEnumC ThreshToZeroInv)
{-# INLINE thresholdToZeroOtsuInv #-}
