{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, ScopedTypeVariables #-}
-- |Array operations.
module AI.CV.OpenCV.ArrayOps (subRS, absDiff, convertScale, 
                              cvAnd, cvAndMask, cvScaleAdd, cvAndS,
                              cvMul, cvMul', cvAdd, cvAddS) where
import Data.Word (Word8)
import Foreign.C.Types (CDouble)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore (CvArr, IplImage)
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp

foreign import ccall unsafe "opencv2/core/core_c.h cvSubRS"
  c_cvSubRS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Compute @value - src[i]@ for every pixel in the source 'HIplImage'.
subRS :: (HasChannels c, HasDepth d, HasScalar c d, 
          IsCvScalar s, s ~ CvScalar c d) =>
         s -> HIplImage c d -> HIplImage c d
subRS value = cv2 $ \src dst -> 
              c_cvSubRS (castPtr src) r g b a (castPtr dst) nullPtr
    where (r,g,b,a) = toCvScalar value
{-# INLINE subRS #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvAbsDiff"
  c_cvAbsDiff :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the absolute difference between two images.
absDiff :: (HasChannels c, HasDepth d) => 
           HIplImage c d -> HIplImage c d -> HIplImage c d
absDiff src1 = cv2 $ \src2 dst -> 
               withHIplImage src1 $ \src1' ->
                 c_cvAbsDiff (castPtr src1') (castPtr src2) (castPtr dst)
{-# INLINE absDiff #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvConvertScale"
  c_cvConvertScale :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()

-- |Converts one array to another with optional affine
-- transformation. Each element of the source array is multiplied by
-- the @scale@ factor and added to the @shift@ value before being
-- converted to the destination type with rounding and saturation. All
-- the channels of multi-channel arrays are processed
-- independentally. Parameters are @scale@, @shift@, and the source
-- 'HIplImage'.
convertScale :: (HasChannels c, HasDepth d1, HasDepth d2) =>
                Double -> Double -> HIplImage c d1 -> 
                HIplImage c d2
convertScale scale shift src = unsafePerformIO $
                               do dst <- mkHIplImage (width src) (height src)
                                  withHIplImage src $ \src' ->
                                    withHIplImage dst $ \dst' ->
                                      c_cvConvertScale (castPtr src') 
                                                       (castPtr dst') 
                                                       (realToFrac scale) 
                                                       (realToFrac shift)
                                  return dst
{-# NOINLIN convertScale #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvAnd"
  c_cvAnd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

cvAndHelper :: Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> 
               IO ()
cvAndHelper src1 src2 dst mask = c_cvAnd (castPtr src1) (castPtr src2)
                                         (castPtr dst) (castPtr mask)

-- |Calculate the per-element bitwise conjunction of two
-- arrays. Parameters are a mask and two source images. The mask
-- specifies the elements of the result that will be computed via the
-- conjunction, and those that will simply be copied from the third
-- parameter.
cvAndMask :: (HasChannels c, HasDepth d) => 
             HIplImage MonoChromatic Word8 -> HIplImage c d ->  
             HIplImage c d -> HIplImage c d
cvAndMask mask src1 = cv2 $ \src2 dst -> 
                      withHIplImage src1 $ \src1' -> 
                          withHIplImage mask $ \mask' ->
                              cvAndHelper src1' src2 dst mask'
{-# INLINE cvAndMask #-}

-- |Calculates the per-element bitwise conjunction of two arrays.
cvAnd :: (HasChannels c, HasDepth d) => 
          HIplImage c d -> HIplImage c d ->  HIplImage c d
cvAnd src1 = cv2 $ \src2 dst -> withHIplImage src1 $ \src1' -> 
             cvAndHelper src1' src2 dst nullPtr
{-# INLINE cvAnd #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvAndS"
   c_cvAndS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise conjunction of an array and a scalar. 
cvAndS :: (HasChannels c, HasDepth d, HasScalar c d, IsCvScalar s, 
           s ~ CvScalar c d) => 
          s -> HIplImage c d -> HIplImage c d
cvAndS s = cv2 $ \img dst -> 
           c_cvAndS (castPtr img) r g b a (castPtr dst) nullPtr
    where (r,g,b,a) = toCvScalar s
{-# INLINE cvAndS #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvScaleAdd"
  c_cvScaleAdd :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
                  Ptr CvArr -> Ptr CvArr -> IO ()

cvScaleAdd :: (HasScalar c d, HasDepth d, HasChannels c, 
               s ~ CvScalar c d, IsCvScalar s) => 
              HIplImage c d -> s -> HIplImage c d -> HIplImage c d
cvScaleAdd src1 s = cv2 $ \src2 dst ->
                    withHIplImage src1 $ \src1' ->
                        c_cvScaleAdd (castPtr src1') r g b a 
                                     (castPtr src2) (castPtr dst)
    where (r,g,b,a) = toCvScalar s
{-# INLINE cvScaleAdd #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvMul"
  c_cvMul :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> CDouble -> IO ()

cvMulHelper :: Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> Double -> IO ()
cvMulHelper src1 src2 dst s = c_cvMul (castPtr src1) (castPtr src2) 
                                      (castPtr dst) (realToFrac s)

-- |Per-element product of two arrays.
cvMul :: (HasChannels c, HasDepth d) => 
         HIplImage c d -> HIplImage c d -> HIplImage c d
cvMul src1 = cv2 $ \src2 dst -> 
             withHIplImage src1 $ \src1' ->
                 cvMulHelper src1' src2 dst 1
{-# INLINE cvMul #-}

-- |Per-element product of two arrays with an extra scale factor that
-- is multiplied with each product.
cvMul' :: (HasChannels c, HasDepth d) => 
          Double -> HIplImage c d -> HIplImage c d -> HIplImage c d
cvMul' scale src1 = cv2 $ \src2 dst ->
                    withHIplImage src1 $ \src1' ->
                        cvMulHelper src1' src2 dst scale
{-# INLINE cvMul' #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvAdd"
  c_cvAdd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element sum of two arrays.
cvAdd :: (HasChannels c, HasDepth d) => 
         HIplImage c d -> HIplImage c d -> HIplImage c d
cvAdd src1 = cv2 $ \src2 dst ->
             withHIplImage src1 $ \src1' ->
                 c_cvAdd (castPtr src1') (castPtr src2) (castPtr dst) nullPtr
{-# INLINE cvAdd #-}

foreign import ccall unsafe "opencv2/core/core_c.h cvAddS"
  c_cvAddS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
              Ptr CvArr -> Ptr CvArr -> IO ()

cvAddS :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d) => 
          s -> HIplImage c d -> HIplImage c d
cvAddS scalar = cv2 $ \src dst -> 
                c_cvAddS (castPtr src) r g b a (castPtr dst) nullPtr
    where (r,g,b,a) = toCvScalar scalar
{-# INLINE cvAddS #-}
