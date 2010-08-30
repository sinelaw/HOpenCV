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
import AI.CV.OpenCV.Core.HIplUtils

foreign import ccall unsafe "opencv/cxcore.h cvSubRS"
  c_cvSubRS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Compute @value - src[i]@ for every pixel in the source 'HIplImage'.
subRS :: (HasChannels c, HasDepth d, HasScalar c d, 
          IsCvScalar s, s ~ CvScalar c d) =>
         s -> HIplImage c d -> HIplImage c d
subRS value src = unsafePerformIO $ 
                  withHIplImage src $ \srcPtr ->
                    return . fst . withCompatibleImage src $ \dstPtr -> 
                      c_cvSubRS (castPtr srcPtr) r g b a (castPtr dstPtr) 
                                nullPtr
    where (r,g,b,a) = toCvScalar value

-- Unsafe in-place pointwise subtraction of each pixel from a given
-- scalar value.
unsafeSubRS :: (HasChannels c, HasDepth d, HasScalar c d,
                IsCvScalar s, s ~ CvScalar c d) =>
               s -> HIplImage c d -> IO (HIplImage c d)
unsafeSubRS value src = withHIplImage src $ \srcPtr ->
                            do c_cvSubRS (castPtr srcPtr) r g b a
                                         (castPtr srcPtr) nullPtr
                               return src
    where (r,g,b,a) = toCvScalar value

{-# RULES "subRS/in-place" forall v. subRS v = pipeline (unsafeSubRS v)
  #-}

foreign import ccall unsafe "opencv/cxcore.h cvAbsDiff"
  c_cvAbsDiff :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the absolute difference between two images.
absDiff :: (HasChannels c, HasDepth d) => 
           HIplImage c d -> HIplImage c d -> HIplImage c d
absDiff src1 src2 = unsafePerformIO $
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                        return . fst . withCompatibleImage src1 $ \dst ->
                          c_cvAbsDiff (castPtr src1') (castPtr src2') 
                                      (castPtr dst)

unsafeAbsDiff :: (HasChannels c, HasDepth d) => 
                 HIplImage c d -> HIplImage c d -> IO (HIplImage c d)
unsafeAbsDiff src1 src2 = withHIplImage src1 $ \src1' ->
                            withHIplImage src2 $ \src2' ->
                                do c_cvAbsDiff (castPtr src1') (castPtr src2') 
                                               (castPtr src2')
                                   return src2

{-# RULES "absDiff/in-place" forall m. absDiff m = pipeline (unsafeAbsDiff m) 
  #-}

foreign import ccall unsafe "opencv/cxcore.h cvConvertScale"
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

foreign import ccall unsafe "opencv/cxcore.h cvAnd"
  c_cvAnd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

cvAndAux :: Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> IO ()
cvAndAux src1 src2 dst mask = c_cvAnd (castPtr src1) (castPtr src2)
                                      (castPtr dst) (castPtr mask)

-- |Calculate the per-element bitwise conjunction of two
-- arrays. Parameters are a mask and two source images. The mask
-- specifies the elements of the result that will be computed via the
-- conjunction, and those that will simply be copied from the third
-- parameter.
cvAndMask :: (HasChannels c, HasDepth d) => 
             HIplImage MonoChromatic Word8 -> HIplImage c d ->  
             HIplImage c d -> HIplImage c d
cvAndMask mask src1 src2 = fst . withDuplicateImage src2 $ \dst ->
                             withHIplImage src1 $ \src1' ->
                               withHIplImage src2 $ \src2' ->
                                 withHIplImage mask $ \mask' ->
                                   cvAndAux src1' src2' dst mask'

-- |Calculates the per-element bitwise conjunction of two arrays.
cvAnd :: (HasChannels c, HasDepth d) => 
          HIplImage c d -> HIplImage c d ->  HIplImage c d
cvAnd src1 src2 = fst . withCompatibleImage src1 $ \dst ->
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                        cvAndAux src1' src2' dst nullPtr

unsafeAnd :: (HasChannels c, HasDepth d) => 
             HIplImage c d -> HIplImage c d -> IO (HIplImage c d)
unsafeAnd src1 src2 = withHIplImage src1 $ \src1' ->
                        withHIplImage src2 $ \src2' ->
                          cvAndAux src1' src2' src2' nullPtr >> return src2

unsafeAndMask :: (HasChannels c, HasDepth d) => 
                 HIplImage MonoChromatic Word8 -> HIplImage c d ->  
                 HIplImage c d -> IO (HIplImage c d)
unsafeAndMask mask src1 src2 = withHIplImage src1 $ \src1' ->
                                 withHIplImage src2 $ \src2' ->
                                   withHIplImage mask $ \mask' ->
                                     cvAndAux src1' src2' src2' mask' >> 
                                     return src2

{-# RULES 
"cvAnd/in-place" forall s. cvAnd s = pipeline (unsafeAnd s)
"cvAndMask/in-place" forall m s. cvAndMask m s = pipeline (unsafeAndMask m s)
  #-}

foreign import ccall unsafe "opencv/cxcore.h cvAndS"
   c_cvAndS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise conjunction of an array and a scalar. 
cvAndS :: (HasChannels c, HasDepth d, HasScalar c d, IsCvScalar s, 
           s ~ CvScalar c d) => 
          s -> HIplImage c d -> HIplImage c d
cvAndS s img = fst . withCompatibleImage img $ \dst ->
                 withHIplImage img $ \src ->
                   c_cvAndS (castPtr src) r g b a (castPtr dst) nullPtr
    where (r,g,b,a) = toCvScalar s

unsafeAndS :: (HasChannels c, HasDepth d, HasScalar c d, IsCvScalar s, 
               s ~ CvScalar c d) => 
              s -> HIplImage c d -> IO (HIplImage c d)
unsafeAndS s img = do withHIplImage img $ \src ->
                        c_cvAndS (castPtr src) r g b a (castPtr src) nullPtr
                      return img
    where (r,g,b,a) = toCvScalar s

{-# RULES "cvAndS/in-place" forall s. cvAndS s = pipeline (unsafeAndS s) #-}

foreign import ccall unsafe "opencv/cxcore.h cvScaleAdd"
  c_cvScaleAdd :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
                  Ptr CvArr -> Ptr CvArr -> IO ()

cvScaleAdd :: (HasScalar c d, HasDepth d, HasChannels c, 
               s ~ CvScalar c d, IsCvScalar s) => 
              HIplImage c d -> s -> HIplImage c d -> HIplImage c d
cvScaleAdd src1 s src2 = fst . withCompatibleImage src1 $ \dst ->
                           withHIplImage src1 $ \src1' ->
                             withHIplImage src2 $ \src2' ->
                               c_cvScaleAdd (castPtr src1') r g b a 
                                            (castPtr src2') (castPtr dst)
    where (r,g,b,a) = toCvScalar s

foreign import ccall unsafe "opencv/cxcore.h cvMul"
  c_cvMul :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> CDouble -> IO ()

cvMulAux :: Ptr IplImage -> Ptr IplImage -> Ptr IplImage -> Double -> IO ()
cvMulAux src1 src2 dst s = c_cvMul (castPtr src1) (castPtr src2) 
                                   (castPtr dst) (realToFrac s)

-- |Per-element product of two arrays.
cvMul :: (HasChannels c, HasDepth d) => 
         HIplImage c d -> HIplImage c d -> HIplImage c d
cvMul src1 src2 = fst . withCompatibleImage src1 $ \dst ->
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                        cvMulAux src1' src2' dst 1

-- |Per-element product of two arrays with an extra scale factor that
-- is multiplied with each product.
cvMul' :: (HasChannels c, HasDepth d) => 
          Double -> HIplImage c d -> HIplImage c d -> HIplImage c d
cvMul' scale src1 src2 = fst . withCompatibleImage src1 $ \dst ->
                           withHIplImage src1 $ \src1' ->
                               withHIplImage src2 $ \src2' ->
                                   cvMulAux src1' src2' dst scale

unsafeMul :: (HasChannels c, HasDepth d) => 
             HIplImage c d -> HIplImage c d -> IO (HIplImage c d)
unsafeMul src1 src2 = do withHIplImage src1 $ \src1' ->
                             withHIplImage src2 $ \src2' ->
                                 cvMulAux src1' src2' src2' 1
                         return src2

unsafeMul' :: (HasChannels c, HasDepth d) => 
              Double -> HIplImage c d -> HIplImage c d -> IO (HIplImage c d)
unsafeMul' scale src1 src2 = do withHIplImage src1 $ \src1' ->
                                  withHIplImage src2 $ \src2' ->
                                    cvMulAux src1' src2' src2' scale
                                return src2

{-# RULES 
"cvMul/in-place" forall s1. cvMul s1 = pipeline (unsafeMul s1)
"cvMul'/in-place" forall s s1. cvMul' s s1 = pipeline (unsafeMul' s s1)
  #-}

foreign import ccall unsafe "opencv/cxcore.h cvAdd"
  c_cvAdd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element sum of two arrays.
cvAdd :: (HasChannels c, HasDepth d) => 
         HIplImage c d -> HIplImage c d -> HIplImage c d
cvAdd src1 src2 = fst . withCompatibleImage src1 $ \dst ->
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                         c_cvAdd (castPtr src1') (castPtr src2') 
                                 (castPtr dst) nullPtr

unsafeAdd  :: (HasChannels c, HasDepth d) => 
              HIplImage c d -> HIplImage c d -> IO (HIplImage c d)
unsafeAdd src1 src2 = do withHIplImage src1 $ \src1' ->
                           withHIplImage src2 $ \src2' ->
                             c_cvAdd (castPtr src1') (castPtr src2') 
                                     (castPtr src2') nullPtr
                         return src2

foreign import ccall unsafe "opencv/cxcore.h cvAddS"
  c_cvAddS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
              Ptr CvArr -> Ptr CvArr -> IO ()

cvAddS :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d) => 
          s -> HIplImage c d -> HIplImage c d
cvAddS scalar src = fst . withCompatibleImage src $ \dst ->
                      withHIplImage src $ \src' ->
                        c_cvAddS (castPtr src') r g b a (castPtr dst) nullPtr
    where (r,g,b,a) = toCvScalar scalar

unsafeAddS :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d) => 
              s -> HIplImage c d -> IO (HIplImage c d)
unsafeAddS scalar src = do withHIplImage src $ \src' ->
                             c_cvAddS (castPtr src') r g b a (castPtr src') nullPtr
                           return src
    where (r,g,b,a) = toCvScalar scalar

{-# RULES
"cvAdd/in-place" forall s. cvAdd s = pipeline (unsafeAdd s)
"cvAddS/in-place" forall s. cvAddS s = pipeline (unsafeAddS s)
  #-}
