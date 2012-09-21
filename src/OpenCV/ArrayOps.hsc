{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, ScopedTypeVariables,
             FlexibleContexts, DataKinds, ConstraintKinds #-}
-- |Array operations.
module OpenCV.ArrayOps (subRS, absDiff, abs, convertScale, 
                        cvAnd, andMask, scaleAdd, cvAndS,
                        cvOr, cvOrS, set, cvAbs, cvAbsDiffS,
                        mul, mulS, add, addS, sub, subMask,
                        cmpS, avg, avgMask, cvNot, withROI,
                        ComparisonOp(..), isolateChannel, copy,
                        replaceChannel, convertScaleAbs, absSat) where
import Data.Word (Word8)
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable (poke, peek)
import System.IO.Unsafe (unsafePerformIO)
import OpenCV.Core.CxCore (CvArr, CvRect(..), CmpOp(..), CvScalar(..),
                           cmpEq, cmpGT, cmpGE, cmpLT, cmpLE, cmpNE)
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp
import OpenCV.Core.StorableUtil

type M = Monochromatic

#include <opencv2/core/core_c.h>

#def void c_cvSubRS(CvArr* src1, CvScalar* src2, CvArr* dst, const CvArr* mask) { cvSubRS(src1, *src2, dst, mask); }

-- foreign import ccall "opencv2/core/core_c.h cvSubRS"
--   c_cvSubRS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--                Ptr CvArr -> Ptr CvArr -> IO ()
foreign import ccall "" -- "static ArrayOps_hsc.h c_cvSubRS"
  c_cvSubRS :: Ptr CvArr -> Ptr CvScalar -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |@subRS value src@ computes @value - src[i]@ for every pixel.
subRS :: (HasDepth d, ScalarOK s c d, Inplace r c d c d) =>
         s -> Image c d r -> Image c d r
subRS value = cv2 $ \src dst -> 
              withS (toCvScalar value) $ \sPtr ->
                c_cvSubRS src sPtr dst nullPtr
{-# INLINE subRS #-}

foreign import ccall "opencv2/core/core_c.h cvAbsDiff"
  c_cvAbsDiff :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the absolute difference between two images.
absDiff :: (HasDepth d, Inplace r c d c d) => 
           Image c d r -> Image c d r -> Image c d r
absDiff src1 = cv2 $ \src2 dst -> 
               withIplImage src1 $ \src1' ->
                 c_cvAbsDiff (castPtr src1') src2 dst
{-# INLINE absDiff #-}

#def void c_cvAbsDiffS(const CvArr* src, CvArr* dst, CvScalar* value) {\
  cvAbsDiffS(src, dst, *value);\
}

-- foreign import ccall "opencv2/core/core_c.h cvAbsDiffS"
--   c_cvAbsDiffS :: Ptr CvArr -> Ptr CvArr -> 
--                   CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall ""
  c_cvAbsDiffS :: Ptr CvArr -> Ptr CvArr -> Ptr CvScalar -> IO ()

-- |Absolute difference of each pixel in an image and a scalar.
cvAbsDiffS :: (HasDepth d, Inplace r c d c d, ScalarOK s c d) =>
              s -> Image c d r -> Image c d r
cvAbsDiffS value = cv2 $ \src dst -> 
                   withS (toCvScalar value) $ \vPtr ->
                     c_cvAbsDiffS src dst vPtr
{-# INLINE cvAbsDiffS #-}

-- |Absolute value of each pixel.
cvAbs :: (HasDepth d, Inplace r c d c d) => Image c d r -> Image c d r
cvAbs = cv2 $ \src dst -> 
        withS (CvScalar 0 0 0 0) $ \sPtr -> 
          c_cvAbsDiffS src dst sPtr
{-# INLINE cvAbs #-}

foreign import ccall "opencv2/core/core_c.h cvConvertScale"
  c_cvConvertScale :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()

-- |Converts one array to another with optional affine
-- transformation. Each element of the source array is multiplied by
-- the @scale@ factor and added to the @shift@ value before being
-- converted to the destination type with rounding and saturation. All
-- the channels of multi-channel arrays are processed
-- independentally. Parameters are @scale@, @shift@, and the source
-- 'Image'.
convertScale :: (HasDepth d1, HasDepth d2, Inplace r c d1 c d2) =>
                Double -> Double -> Image c d1 r -> Image c d2 r
convertScale scale shift = cv2 $ \src dst -> 
                           c_cvConvertScale src dst (rf scale) (rf shift)
  where rf = realToFrac
{-# INLINE convertScale #-}

foreign import ccall "opencv2/core/core_c.h cvConvertScaleAbs"
  c_cvConvertScaleAbs :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()

-- |@convertScaleAbs scale shift@ scales each element of an image,
-- adds an offset to the scaled value, computes the absolute value,
-- and saturates to 8 bits.
convertScaleAbs :: (HasDepth d, Inplace r c d c Word8) =>
                   CDouble -> CDouble -> Image c d r -> Image c Word8 r
convertScaleAbs scale shift = cv2 $ \src dst -> 
                              c_cvConvertScaleAbs src dst scale shift
{-# INLINE convertScaleAbs #-}

-- |Computes the absolute value of each pixel and saturates to 8 bits.
absSat :: (HasDepth d, Inplace r c d c Word8) =>
          Image c d r -> Image c Word8 r
absSat = convertScaleAbs 1 0
{-# INLINE absSat #-}

foreign import ccall "opencv2/core/core_c.h cvAnd"
  c_cvAnd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the per-element bitwise conjunction of two
-- arrays. Parameters are a mask and two source images. The mask
-- specifies the elements of the result that will be computed via the
-- conjunction, and those that will simply be copied from the third
-- parameter.
andMask :: (HasDepth d, Inplace r3 c d c d) => 
           Image Monochromatic Word8 r1 -> Image c d r2 ->  
           Image c d r3 -> Image c d r3
andMask mask src1 = cv2 $ \src2 dst -> 
                    withIplImage src1 $ \src1' -> 
                        withIplImage mask $ \mask' ->
                            c_cvAnd (castPtr src1') src2 dst (castPtr mask')
{-# INLINE andMask #-}

-- |Calculates the per-element bitwise conjunction of two arrays.
cvAnd :: (HasDepth d, Inplace r2 c d c d) => 
          Image c d r1 -> Image c d r2 ->  Image c d r2
cvAnd src1 = cv2 $ \src2 dst -> withIplImage src1 $ \src1' -> 
             c_cvAnd (castPtr src1') src2 dst nullPtr
{-# INLINE cvAnd #-}

#def void c_cvAndS(const CvArr* src, CvScalar* value, CvArr* dst, const CvArr* mask) { cvAndS(src, *value, dst, mask); }

-- foreign import ccall "opencv2/core/core_c.h cvAndS"
--    c_cvAndS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--                Ptr CvArr -> Ptr CvArr -> IO ()
foreign import ccall ""
   c_cvAndS :: Ptr CvArr -> Ptr CvScalar -> Ptr CvArr -> Ptr CvArr -> IO ()


-- |Per-element bit-wise conjunction of an array and a scalar. 
cvAndS :: (HasDepth d, ScalarOK s c d, Inplace r c d c d) => 
          s -> Image c d r -> Image c d r
cvAndS s = cv2 $ \img dst -> 
           withS (toCvScalar s) $ \sPtr ->
             c_cvAndS img sPtr dst nullPtr
{-# INLINE cvAndS #-}

#def void c_cvScaleAdd(const CvArr* src1, CvScalar* scale,\
                       const CvArr* src2, CvArr* dst) {\
  cvScaleAdd(src1, *scale, src2, dst);\
}

-- foreign import ccall "opencv2/core/core_c.h cvScaleAdd"
--   c_cvScaleAdd :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--                   Ptr CvArr -> Ptr CvArr -> IO ()
foreign import ccall ""
  c_cvScaleAdd :: Ptr CvArr -> Ptr CvScalar -> Ptr CvArr -> Ptr CvArr -> IO ()


-- |Calculate the sum of a scaled array and another array. @scaleAdd
-- src1 s src2@ computes @dst[i] = s*src1[i] + src2[i]@
scaleAdd :: (ScalarOK s c d, HasDepth d, Inplace r2 c d c d) => 
            Image c d r1 -> s -> Image c d r2 -> Image c d r2
scaleAdd src1 s = cv2 $ \src2 dst ->
                  withIplImage src1 $ \src1' ->
                  withS (toCvScalar s) $ \sPtr ->
                    c_cvScaleAdd (castPtr src1') sPtr src2 dst
{-# INLINE scaleAdd #-}

foreign import ccall "opencv2/core/core_c.h cvMul"
  c_cvMul :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> CDouble -> IO ()

cvMulHelper :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Double -> IO ()
cvMulHelper src1 src2 dst s = c_cvMul src1 src2 dst (realToFrac s)

-- |Per-element product of two arrays.
mul :: (HasDepth d, Inplace r2 c d c d) => 
       Image c d r1 -> Image c d r2 -> Image c d r2
mul src1 = cv2 $ \src2 dst -> 
           withIplImage src1 $ \src1' ->
               cvMulHelper (castPtr src1') src2 dst 1
{-# INLINE mul #-}

-- |Per-element product of two arrays with an extra scale factor that
-- is multiplied with each product.
mulS :: (HasDepth d, Inplace r2 c d c d) => 
        Double -> Image c d r1 -> Image c d r2 -> Image c d r2
mulS scale src1 = cv2 $ \src2 dst ->
                  withIplImage src1 $ \src1' ->
                      cvMulHelper (castPtr src1') src2 dst scale
{-# INLINE mulS #-}

foreign import ccall "opencv2/core/core_c.h cvAdd"
  c_cvAdd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element sum.
add :: (HasDepth d1, HasDepth d2, HasDepth d3, Inplace r2 c d2 c d3) => 
       Image c d1 r1 -> Image c d2 r2 -> Image c d3 r2
add src1 = cv2 $ \src2 dst ->
           withIplImage src1 $ \src1' ->
               c_cvAdd (castPtr src1') src2 dst nullPtr
{-# INLINE add #-}

#def void c_cvAddS(const CvArr* src, CvScalar* value, CvArr* dst,\
                   const CvArr* mask) {\
  cvAddS(src, *value, dst, mask);\
}

-- foreign import ccall "opencv2/core/core_c.h cvAddS"
--   c_cvAddS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--               Ptr CvArr -> Ptr CvArr -> IO ()
foreign import ccall ""
  c_cvAddS :: Ptr CvArr -> Ptr CvScalar -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Computes the sum of an array and a scalar.
addS :: (HasDepth d, ScalarOK s c d, Inplace r c d c d) => 
        s -> Image c d r -> Image c d r
addS scalar = cv2 $ \src dst -> 
              withS (toCvScalar scalar) $ \sPtr ->
                c_cvAddS src sPtr dst nullPtr
{-# INLINE addS #-}

foreign import ccall "opencv2/core/core_c.h cvSub"
  c_cvSub :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element difference.
sub :: (HasDepth d, Inplace r2 c d c d) =>
       Image c d r1 -> Image c d r2 -> Image c d r2
sub img1 = cv2 $ \img2 dst ->
           withIplImage img1 $ \img1' -> 
               c_cvSub (castPtr img1') img2 dst nullPtr
{-# INLINE sub #-}

-- |WARNING: Argument order may be confusing! @cvSubMask img2 mask
-- img1@ computes @dst[i] = img1[i] - img2[i] if mask[i]@. The idea is
-- that @dst@ is the same as @img1@ everywhere @mask@ is zero. This
-- permits in-place updating of @img1@.
subMask :: (HasDepth d, Inplace r3 c d c d, UpdateROI r3) =>
           Image c d r1 -> Image Monochromatic Word8 r2 -> Image c d r3 -> 
           Image c d r3
subMask img2 mask = cv $ \img1 ->
                    withIplImage mask $ \mask' -> 
                        withIplImage img2 $ \img2' ->
                            c_cvSub img1 (castPtr img2') img1 (castPtr mask')
{-# INLINE subMask #-}

foreign import ccall "opencv2/core/core_c.h cvOr"
  c_cvOr :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise disjunction of two arrays
cvOr :: (HasDepth d, Inplace r2 c d c d) =>
        Image c d r1 -> Image c d r2 -> Image c d r2
cvOr img1 = cv2 $ \img2 dst ->
            withIplImage img1 $ \img1' -> 
                c_cvOr (castPtr img1') img2 dst nullPtr
{-# INLINE cvOr #-}

#def void c_cvOrS(const CvArr* src, CvScalar* value, CvArr* dst,\
                  const CvArr* mask) {\
  cvOrS(src, *value, dst, mask);\
}

-- foreign import ccall "opencv2/core/core_c.h cvOrS"
--   c_cvOrS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--              Ptr CvArr -> Ptr CvArr -> IO ()
foreign import ccall ""
  c_cvOrS :: Ptr CvArr -> Ptr CvScalar -> Ptr CvArr -> Ptr CvArr -> IO ()


-- |Per-element bit-wise disjunction of an array and a scalar.
cvOrS :: (HasDepth d, ScalarOK s c d, Inplace r c d c d) => 
         s -> Image c d r -> Image c d r
cvOrS scalar = cv2 $ \src dst -> 
               withS (toCvScalar scalar) $ \sPtr ->
                 c_cvOrS src sPtr dst nullPtr
{-# INLINE cvOrS #-}

#def void c_cvSet(CvArr* src, CvScalar* value, const CvArr* mask) {\
  cvSet(src, *value, mask);\
}

-- foreign import ccall "opencv2/core/core_c.h cvSet"
--   c_cvSet :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
--              Ptr CvArr -> IO ()
foreign import ccall ""
  c_cvSet :: Ptr CvArr -> Ptr CvScalar -> Ptr CvArr -> IO ()

-- |Set every element of an array to a given value.
set :: (HasDepth d, ScalarOK s c d, Inplace r c d c d) => 
       s -> Image c d r -> Image c d r
set scalar img@Image{} = flip cv img $ \src -> 
                         withS (toCvScalar scalar) $ \sPtr ->
                           c_cvSet src sPtr nullPtr 
{-# INLINE set #-}

setROICV :: forall c d r. HasDepth d => 
            CvRect -> Image c d r -> Image c d HasROI
setROICV (CvRect x y w h) = cv $ \img -> c_cvSetImageROI img x y w h
{-# INLINE setROICV #-}

resetROICV :: forall c d r. HasDepth d => Image c d r -> Image c d NoROI
resetROICV = cv $ \img -> c_cvResetImageROI img
{-# INLINE resetROICV #-}

-- |Restrict an operation to a specific region-of-interest. This
-- operation fuses.
withROI :: (HasDepth d1, HasDepth d2) => 
           CvRect -> (Image c1 d1 HasROI -> Image c2 d2 r2) -> 
           Image c1 d1 r -> Image c2 d2 NoROI
withROI r f = resetROICV . f . setROICV r
{-# INLINE withROI #-}

foreign import ccall "opencv2/core/core_c.h cvCmpS"
  c_cvCmpS :: Ptr CvArr -> CDouble -> Ptr CvArr -> CInt -> IO ()

data ComparisonOp = CmpEq | CmpGT | CmpGE | CmpLT | CmpLE | CmpNE

cmpToCmp :: ComparisonOp -> CInt
cmpToCmp CmpEq = unCmpOp cmpEq
cmpToCmp CmpGT = unCmpOp cmpGT
cmpToCmp CmpGE = unCmpOp cmpGE
cmpToCmp CmpLT = unCmpOp cmpLT
cmpToCmp CmpLE = unCmpOp cmpLE
cmpToCmp CmpNE = unCmpOp cmpNE

-- |Per-element comparison of an array and a scalar.
cmpS :: (HasDepth d, Inplace r M d M Word8) => 
        ComparisonOp -> d -> Image Monochromatic d r -> 
        Image Monochromatic Word8 r
cmpS op v = cv2 $ \src dst ->
            c_cvCmpS src v' dst (cmpToCmp op)
    where v' = realToFrac . toDouble $ v
{-# INLINE cmpS #-}

foreign import ccall "HOpenCV_wrap.h c_cvAvg"
  c_cvAvg :: Ptr CvArr -> Ptr CvArr -> Ptr CvScalar -> IO ()

avgWorker :: AsCvScalar b => Ptr CvArr -> Ptr CvArr -> IO b
avgWorker img mask = alloca $ \ptr ->
                     c_cvAvg img mask ptr >> fromCvScalar `fmap` peek ptr
-- avgWorker img mask = allocaArray 4 $ 
--                      \arr -> do c_cvAvg img mask arr
--                                 fromCvScalar `fmap` peek arr
--                                 -- [r,g,b,a] <- peekArray 4 arr
--                                 -- return $ fromCvScalar (r,g,b,a)

-- |Calculates the mean independently for each channel.
avg :: (HasDepth d, ScalarOK s c d) => Image c d r -> CvScalarT c d
avg img = unsafePerformIO . withIplImage img $ flip avgWorker nullPtr . castPtr
{-# NOINLINE avg #-}

-- |@avgMask img mask@ calculates the mean independently for each
-- channel for each element of the source array whose entry in @mask@
-- is non-zero.
avgMask :: (HasDepth d, ScalarOK s c d) => 
           Image c d r1 -> Image Monochromatic Word8 r2 -> CvScalarT c d
avgMask img mask = unsafePerformIO . withIplImage img $ \src ->
                   withIplImage mask $ avgWorker (castPtr src) . castPtr
{-# NOINLINE avgMask #-}

foreign import ccall "opencv2/core/core_c.h cvNot"
  c_cvNot :: Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise inversion.
cvNot :: (HasDepth d, Inplace r c d c d) => 
         Image c d r -> Image c d r
cvNot = cv2 $ \src dst -> c_cvNot src dst
{-# INLINE cvNot #-}

foreign import ccall "opencv2/core/core_c.h cvMixChannels"
  cvMixChannels :: Ptr (Ptr CvArr) -> CInt -> Ptr (Ptr CvArr) -> CInt -> 
                   Ptr CInt -> CInt -> IO ()

-- |Isolate a specific channel from a trichromatic image.
isolateChannel :: (HasDepth d, Inplace r Trichromatic d M d) =>
                  CInt -> Image Trichromatic d r -> Image Monochromatic d r
isolateChannel n = cv2 $ \src dst -> 
                   alloca $ \p1 -> poke p1 src >>
                                   (alloca $ \p2 ->
                                     poke p2 dst >>
                                     (withArray [n,0] $ \ft ->
                                       cvMixChannels p1 1 p2 1 ft 1))
{-# INLINE isolateChannel #-}

-- |Replace a specific channel of a trichromatic image with the single
-- channel from a monochromatic image.
replaceChannel :: (HasDepth d, Inplace r2 Trichromatic d Trichromatic d) => 
                  CInt -> Image Monochromatic d r1 -> 
                  Image Trichromatic d r2 -> Image Trichromatic d r2
replaceChannel n c = cv2 $ \src dst -> 
                     withIplImage c $ \cp ->
                       withArray [castPtr cp, src] $ \p1 -> 
                         withArray [dst] $ \p2 ->
                           withArray [0,n,1+n',n',1+n'',n''] $ \ft ->
                             cvMixChannels p1 2 p2 1 ft 3
  where n' = (n + 1) `rem` 3
        n'' = (n + 2) `rem` 3
{-# INLINE replaceChannel #-}

foreign import ccall "opencv2/core/core_c.h cvCopy"
  cvCopy :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

copy :: (UpdateROI r2, HasDepth d) => 
        Image c d r1 -> Image c d r2 -> Image c d r2
copy src = cv $ \dst -> withIplImage src $ \src' ->
           cvCopy (castPtr src') dst nullPtr
{-# INLINE copy #-}
