{-# LANGUAGE ForeignFunctionInterface #-}
-- |Array operations.
module AI.CV.OpenCV.ArrayOps (subRS, subRSVec, absDiff, convertScale, 
                              cvAnd, cvAndMask) where
import Data.Word (Word8)
import Foreign.C.Types (CDouble)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore (CvArr, IplImage)
import AI.CV.OpenCV.Core.HIplUtils

foreign import ccall unsafe "opencv/cxcore.h cvSubRS"
  c_cvSubRS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Compute @value - src[i]@ for every pixel in the source 'HIplImage'.
subRS :: (HasDepth d, Storable d) =>
         d -> HIplImage a MonoChromatic d -> 
         HIplImage FreshImage MonoChromatic d
subRS value src = unsafePerformIO $ 
                  withHIplImage src $ \srcPtr ->
                    return . fst . withCompatibleImage src $ \dstPtr -> 
                      c_cvSubRS (castPtr srcPtr) v v v v (castPtr dstPtr) 
                                nullPtr
    where v = realToFrac . toDouble $ value

-- Unsafe in-place pointwise subtraction of each pixel from a given
-- scalar value.
unsafeSubRS :: (HasDepth d, Storable d) =>
               d -> HIplImage FreshImage MonoChromatic d ->
               HIplImage FreshImage MonoChromatic d
unsafeSubRS value src = unsafePerformIO $
                        withHIplImage src $ \srcPtr ->
                            do c_cvSubRS (castPtr srcPtr) v v v v 
                                         (castPtr srcPtr) nullPtr
                               return src
    where v = realToFrac . toDouble $ value

{-# RULES "subRS-in-place" forall v (f::a -> HIplImage FreshImage MonoChromatic d). 
    subRS v . f = unsafeSubRS v . f
  #-}

-- |Compute @value - src[i]@ for every pixel in the source 'HIplImage'.
subRSVec :: (HasDepth d, Storable d) =>
            (d,d,d) -> HIplImage a TriChromatic d ->
            HIplImage FreshImage TriChromatic d
subRSVec (r,g,b) src = unsafePerformIO $
                       withHIplImage src $ \src' ->
                         return . fst . withCompatibleImage src $ \dst' ->
                           c_cvSubRS (castPtr src') r' g' b' 0 (castPtr dst')
                                     nullPtr
    where r' = realToFrac . toDouble $ r
          g' = realToFrac . toDouble $ g
          b' = realToFrac . toDouble $ b

unsafeSubRSVec :: (HasDepth d, Storable d) =>
                  (d,d,d) -> HIplImage FreshImage TriChromatic d ->
                  HIplImage FreshImage TriChromatic d
unsafeSubRSVec (r,g,b) src = unsafePerformIO $
                             withHIplImage src $ \src' ->
                                 do c_cvSubRS (castPtr src') r' g' b' 0 
                                              (castPtr src') nullPtr
                                    return src
    where r' = realToFrac . toDouble $ r
          g' = realToFrac . toDouble $ g
          b' = realToFrac . toDouble $ b

{-# RULES "subRSVec-inplace" 
  forall v (g::a->HIplImage FreshImage TriChromatic d).
  subRSVec v . g = unsafeSubRSVec v . g
  #-}

foreign import ccall unsafe "opencv/cxcore.h cvAbsDiff"
  c_cvAbsDiff :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the absolute difference between two images.
absDiff :: (HasChannels c, HasDepth d, Storable d) => 
           HIplImage a c d -> HIplImage a c d -> HIplImage FreshImage c d
absDiff src1 src2 = unsafePerformIO $
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                        return . fst . withCompatibleImage src1 $ \dst ->
                          c_cvAbsDiff (castPtr src1') (castPtr src2') 
                                      (castPtr dst)

unsafeAbsDiff :: (HasChannels c, HasDepth d, Storable d) => 
                 HIplImage a c d -> HIplImage FreshImage c d -> 
                 HIplImage FreshImage c d
unsafeAbsDiff src1 src2 = unsafePerformIO $
                          withHIplImage src1 $ \src1' ->
                            withHIplImage src2 $ \src2' ->
                                do c_cvAbsDiff (castPtr src1') (castPtr src2') 
                                               (castPtr src2')
                                   return src2

{-# RULES "absDiff-inplace"
  forall m1 (g::a -> HIplImage FreshImage c d). 
  absDiff m1 . g = unsafeAbsDiff m1 . g
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
convertScale :: (HasChannels c, HasDepth d1, HasDepth d2, 
                 Storable d1, Storable d2) =>
                Double -> Double -> HIplImage a c d1 -> 
                HIplImage FreshImage c d2
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
cvAndMask :: (HasChannels c, HasDepth d, Storable d) => 
             HIplImage q MonoChromatic Word8 -> HIplImage a c d ->  
             HIplImage b c d -> HIplImage FreshImage c d
cvAndMask mask src1 src2 = fst . withDuplicateImage src2 $ \dst ->
                             withHIplImage src1 $ \src1' ->
                               withHIplImage src2 $ \src2' ->
                                 withHIplImage mask $ \mask' ->
                                   cvAndAux src1' src2' dst mask'

-- |Calculates the per-element bitwise conjunction of two arrays.
cvAnd :: (HasChannels c, HasDepth d, Storable d) => 
          HIplImage a c d -> HIplImage b c d ->  HIplImage FreshImage c d
cvAnd src1 src2 = fst . withCompatibleImage src1 $ \dst ->
                    withHIplImage src1 $ \src1' ->
                      withHIplImage src2 $ \src2' ->
                        cvAndAux src1' src2' dst nullPtr

unsafeAnd :: (HasChannels c, HasDepth d, Storable d) => 
             HIplImage a c d -> HIplImage FreshImage c d ->  
             HIplImage FreshImage c d
unsafeAnd src1 src2 = unsafePerformIO $
                      withHIplImage src1 $ \src1' ->
                        withHIplImage src2 $ \src2' ->
                          cvAndAux src1' src2' src2' nullPtr >> return src2

unsafeAndMask :: (HasChannels c, HasDepth d, Storable d) => 
                 HIplImage q MonoChromatic Word8 -> HIplImage a c d ->  
                 HIplImage FreshImage c d -> HIplImage FreshImage c d
unsafeAndMask mask src1 src2 = unsafePerformIO $
                      withHIplImage src1 $ \src1' ->
                        withHIplImage src2 $ \src2' ->
                          withHIplImage mask $ \mask' ->
                            cvAndAux src1' src2' src2' mask' >> return src2

{-# RULES "cvAnd/in-place"
    forall s (g :: a -> HIplImage FreshImage c d). cvAnd s . g = unsafeAnd s . g
  #-}

{-# RULES "cvAndMask/in-place"
    forall m s (g :: a -> HIplImage FreshImage c d).
    cvAndMask m s . g = unsafeAndMask m s . g 
  #-}
