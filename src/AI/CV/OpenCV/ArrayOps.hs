{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, ScopedTypeVariables,
             FlexibleContexts #-}
-- |Array operations.
module AI.CV.OpenCV.ArrayOps (subRS, absDiff, convertScale, 
                              cvAnd, andMask, scaleAdd, cvAndS,
                              cvOr, cvOrS, set, setROI, resetROI,
                              mul, mulS, add, addS, sub, subMask,
                              cmpS, avg, avgMask, cvNot, withROI,
                              ComparisonOp(..), isolateChannel, 
                              replaceChannel) where
import Data.Word (Word8)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)
import AI.CV.OpenCV.Core.CxCore (CvArr, CvRect(..), CmpOp(..), 
                                 cmpEq, cmpGT, cmpGE, cmpLT, cmpLE, cmpNE)
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.HIplImage (addROI, resetROI)
import AI.CV.OpenCV.Core.CVOp

type M = MonoChromatic

foreign import ccall "opencv2/core/core_c.h cvSubRS"
  c_cvSubRS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Compute @value - src[i]@ for every pixel in the source 'HIplImage'.
subRS :: (HasChannels c, HasDepth d, HasScalar c d, 
          IsCvScalar s, s ~ CvScalar c d, InplaceROI r c d c d) =>
         s -> HIplImage c d r -> HIplImage c d r
subRS value = cv2 $ \src dst -> c_cvSubRS src r g b a dst nullPtr
    where (r,g,b,a) = toCvScalar value
{-# INLINE subRS #-}

foreign import ccall "opencv2/core/core_c.h cvAbsDiff"
  c_cvAbsDiff :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the absolute difference between two images.
absDiff :: (HasChannels c, HasDepth d, InplaceROI r c d c d) => 
           HIplImage c d r -> HIplImage c d r -> HIplImage c d r
absDiff src1 = cv2 $ \src2 dst -> 
               withHIplImage src1 $ \src1' ->
                 c_cvAbsDiff (castPtr src1') src2 dst
{-# INLINE absDiff #-}

foreign import ccall "opencv2/core/core_c.h cvConvertScale"
  c_cvConvertScale :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()

-- |Converts one array to another with optional affine
-- transformation. Each element of the source array is multiplied by
-- the @scale@ factor and added to the @shift@ value before being
-- converted to the destination type with rounding and saturation. All
-- the channels of multi-channel arrays are processed
-- independentally. Parameters are @scale@, @shift@, and the source
-- 'HIplImage'.
convertScale :: (HasChannels c, HasDepth d1, HasDepth d2, ImgBuilder r,
                 InplaceROI r c d1 c d2) =>
                Double -> Double -> HIplImage c d1 r -> 
                HIplImage c d2 r
convertScale scale shift = cv2 $ \src dst -> 
                           c_cvConvertScale src dst (rf scale) (rf shift)
  where rf = realToFrac
{-# INLINE convertScale #-}

foreign import ccall "opencv2/core/core_c.h cvAnd"
  c_cvAnd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the per-element bitwise conjunction of two
-- arrays. Parameters are a mask and two source images. The mask
-- specifies the elements of the result that will be computed via the
-- conjunction, and those that will simply be copied from the third
-- parameter.
andMask :: (HasChannels c, HasDepth d, ImgBuilder r1, ImgBuilder r2, 
            InplaceROI r3 c d c d) => 
           HIplImage MonoChromatic Word8 r1 -> HIplImage c d r2 ->  
           HIplImage c d r3 -> HIplImage c d r3
andMask mask src1 = cv2 $ \src2 dst -> 
                    withHIplImage src1 $ \src1' -> 
                        withHIplImage mask $ \mask' ->
                            c_cvAnd (castPtr src1') src2 dst (castPtr mask')
{-# INLINE andMask #-}

-- |Calculates the per-element bitwise conjunction of two arrays.
cvAnd :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) => 
          HIplImage c d r1 -> HIplImage c d r2 ->  HIplImage c d r2
cvAnd src1 = cv2 $ \src2 dst -> withHIplImage src1 $ \src1' -> 
             c_cvAnd (castPtr src1') src2 dst nullPtr
{-# INLINE cvAnd #-}

foreign import ccall "opencv2/core/core_c.h cvAndS"
   c_cvAndS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
               Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise conjunction of an array and a scalar. 
cvAndS :: (HasChannels c, HasDepth d, HasScalar c d, IsCvScalar s, 
           s ~ CvScalar c d, InplaceROI r c d c d) => 
          s -> HIplImage c d r -> HIplImage c d r
cvAndS s = cv2 $ \img dst -> c_cvAndS img r g b a dst nullPtr
    where (r,g,b,a) = toCvScalar s
{-# INLINE cvAndS #-}

foreign import ccall "opencv2/core/core_c.h cvScaleAdd"
  c_cvScaleAdd :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
                  Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculate the sum of a scaled array and another array. @scaleAdd
-- src1 s src2@ computes @dst[i] = s*src1[i] + src2[i]@
scaleAdd :: (HasScalar c d, HasDepth d, HasChannels c, 
             s ~ CvScalar c d, IsCvScalar s, ImgBuilder r1, 
             InplaceROI r2 c d c d) => 
            HIplImage c d r1 -> s -> HIplImage c d r2 -> HIplImage c d r2
scaleAdd src1 s = cv2 $ \src2 dst ->
                  withHIplImage src1 $ \src1' ->
                      c_cvScaleAdd (castPtr src1') r g b a src2 dst
    where (r,g,b,a) = toCvScalar s
{-# INLINE scaleAdd #-}

foreign import ccall "opencv2/core/core_c.h cvMul"
  c_cvMul :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> CDouble -> IO ()

cvMulHelper :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Double -> IO ()
cvMulHelper src1 src2 dst s = c_cvMul src1 src2 dst (realToFrac s)

-- |Per-element product of two arrays.
mul :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) => 
       HIplImage c d r1 -> HIplImage c d r2 -> HIplImage c d r2
mul src1 = cv2 $ \src2 dst -> 
           withHIplImage src1 $ \src1' ->
               cvMulHelper (castPtr src1') src2 dst 1
{-# INLINE mul #-}

-- |Per-element product of two arrays with an extra scale factor that
-- is multiplied with each product.
mulS :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) => 
        Double -> HIplImage c d r1 -> HIplImage c d r2 -> HIplImage c d r2
mulS scale src1 = cv2 $ \src2 dst ->
                  withHIplImage src1 $ \src1' ->
                      cvMulHelper (castPtr src1') src2 dst scale
{-# INLINE mulS #-}

foreign import ccall "opencv2/core/core_c.h cvAdd"
  c_cvAdd :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element sum.
add :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) => 
       HIplImage c d r1 -> HIplImage c d r2 -> HIplImage c d r2
add src1 = cv2 $ \src2 dst ->
           withHIplImage src1 $ \src1' ->
               c_cvAdd (castPtr src1') src2 dst nullPtr
{-# INLINE add #-}

foreign import ccall "opencv2/core/core_c.h cvAddS"
  c_cvAddS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
              Ptr CvArr -> Ptr CvArr -> IO ()

-- |Computes the sum of an array and a scalar.
addS :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d, 
         InplaceROI r c d c d) => 
        s -> HIplImage c d r -> HIplImage c d r
addS scalar = cv2 $ \src dst -> c_cvAddS src r g b a dst nullPtr
    where (r,g,b,a) = toCvScalar scalar
{-# INLINE addS #-}

foreign import ccall "opencv2/core/core_c.h cvSub"
  c_cvSub :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element difference.
sub :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) =>
       HIplImage c d r1 -> HIplImage c d r2 -> HIplImage c d r2
sub img1 = cv2 $ \img2 dst ->
           withHIplImage img1 $ \img1' -> 
               c_cvSub (castPtr img1') img2 dst nullPtr
{-# INLINE sub #-}

-- |WARNING: Argument order may be confusing! @cvSubMask img2 mask
-- img1@ computes @dst[i] = img1[i] - img2[i] if mask[i]@. The idea is
-- that @dst@ is the same as @img1@ everywhere @mask@ is zero. This
-- permits in-place updating of @img1@.
subMask :: (HasChannels c, HasDepth d, ImgBuilder r1, ImgBuilder r2, 
            InplaceROI r3 c d c d) =>
           HIplImage c d r1 -> HIplImage MonoChromatic Word8 r2 -> HIplImage c d r3 -> 
           HIplImage c d r3
subMask img2 mask = cv $ \img1 ->
                    withHIplImage mask $ \mask' -> 
                        withHIplImage img2 $ \img2' ->
                            c_cvSub img1 (castPtr img2') img1 (castPtr mask')
{-# INLINE subMask #-}

foreign import ccall "opencv2/core/core_c.h cvOr"
  c_cvOr :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise disjunction of two arrays
cvOr :: (HasChannels c, HasDepth d, ImgBuilder r1, InplaceROI r2 c d c d) =>
        HIplImage c d r1 -> HIplImage c d r2 -> HIplImage c d r2
cvOr img1 = cv2 $ \img2 dst ->
            withHIplImage img1 $ \img1' -> 
                c_cvOr (castPtr img1') img2 dst nullPtr
{-# INLINE cvOr #-}

foreign import ccall "opencv2/core/core_c.h cvOrS"
  c_cvOrS :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
             Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise disjunction of an array and a scalar.
cvOrS :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d, 
          InplaceROI r c d c d) => 
         s -> HIplImage c d r -> HIplImage c d r
cvOrS scalar = cv2 $ \src dst -> c_cvOrS src r g b a dst nullPtr
    where (r,g,b,a) = toCvScalar scalar
{-# INLINE cvOrS #-}

foreign import ccall "opencv2/core/core_c.h cvSet"
  c_cvSet :: Ptr CvArr -> CDouble -> CDouble -> CDouble -> CDouble -> 
             Ptr CvArr -> IO ()

-- |Per-element bit-wise disjunction of an array and a scalar.
set :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d, 
        InplaceROI r c d c d) => 
       s -> HIplImage c d r -> HIplImage c d r
set scalar = cv $ \src -> c_cvSet src r g b a nullPtr
    where (r,g,b,a) = toCvScalar scalar
{-# INLINE set #-}

-- |Set an image's region-of-interest.
setROI :: (HasChannels c, HasDepth d) => 
          CvRect -> HIplImage c d r -> HIplImage c d HasROI
setROI = addROI
{-# INLINE setROI #-}

setROICV :: forall c d r. (HasChannels c, HasDepth d, ImgBuilder r) => 
            CvRect -> HIplImage c d r -> HIplImage c d HasROI
setROICV (CvRect x y w h) = cv $ \img -> c_cvSetImageROI img x y w h
{-# INLINE setROICV #-}

-- -- |Clear any region-of-interest set for an image.
--resetROI :: (HasChannels c, HasDepth d) => HIplImage c d r -> HIplImage c d NoROI
--resetROI = removeROI
-- {-# INLINE resetROI #-}

resetROICV :: forall c d r. (HasChannels c, HasDepth d, ImgBuilder r) => 
              HIplImage c d r -> HIplImage c d NoROI
resetROICV = cv $ \img -> c_cvResetImageROI img
{-# INLINE resetROICV #-}

-- |Restrict an operation to a specific region-of-interest. This
-- operation fuses.
withROI :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, 
            ImgBuilder r, ImgBuilder r2) => 
           CvRect -> (HIplImage c1 d1 HasROI -> HIplImage c2 d2 r2) -> 
           HIplImage c1 d1 r -> HIplImage c2 d2 NoROI
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
cmpS :: (HasDepth d, InplaceROI r M d M Word8) => 
        ComparisonOp -> d -> HIplImage MonoChromatic d r -> 
        HIplImage MonoChromatic Word8 r
cmpS op v = cv2 $ \src dst ->
            c_cvCmpS src v' dst (cmpToCmp op)
    where v' = realToFrac . toDouble $ v
{-# INLINE cmpS #-}

foreign import ccall "HOpenCV_wrap.h c_cvAvg"
  c_cvAvg :: Ptr CvArr -> Ptr CvArr -> Ptr CDouble -> IO ()

avgWorker :: IsCvScalar b => Ptr CvArr -> Ptr CvArr -> IO b
avgWorker img mask = allocaArray 4 $ 
                     \arr -> do c_cvAvg img mask arr
                                [r,g,b,a] <- peekArray 4 arr
                                return $ fromCvScalar (r,g,b,a)

-- |Calculates the mean independently for each channel.
avg :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d, ImgBuilder r) => 
       HIplImage c d r -> CvScalar c d
avg img = unsafePerformIO . withHIplImage img $ flip avgWorker nullPtr . castPtr
{-# NOINLINE avg #-}

-- |@avgMask img mask@ calculates the mean independently for each
-- channel for each element of the source array whose entry in @mask@
-- is non-zero.
avgMask :: (HasChannels c, HasDepth d, IsCvScalar s, s ~ CvScalar c d, 
            ImgBuilder r1, ImgBuilder r2) => 
           HIplImage c d r1 -> HIplImage MonoChromatic Word8 r2 -> CvScalar c d
avgMask img mask = unsafePerformIO . withHIplImage img $ \src ->
                   withHIplImage mask $ avgWorker (castPtr src) . castPtr
{-# NOINLINE avgMask #-}

foreign import ccall "opencv2/core/core_c.h cvNot"
  c_cvNot :: Ptr CvArr -> Ptr CvArr -> IO ()

-- |Per-element bit-wise inversion.
cvNot :: (HasChannels c, HasDepth d, InplaceROI r c d c d) => 
         HIplImage c d r -> HIplImage c d r
cvNot = cv2 $ \src dst -> c_cvNot src dst
{-# INLINE cvNot #-}

foreign import ccall "opencv2/core/core_c.h cvMixChannels"
  cvMixChannels :: Ptr (Ptr CvArr) -> CInt -> Ptr (Ptr CvArr) -> CInt -> 
                   Ptr CInt -> CInt -> IO ()

-- |Isolate a specific channel from a trichromatic image.
isolateChannel :: (HasDepth d, InplaceROI r TriChromatic d M d) =>
                  CInt -> HIplImage TriChromatic d r -> HIplImage MonoChromatic d r
isolateChannel n = cv2 $ \src dst -> 
                   alloca $ \p1 -> poke p1 src >>
                                   (alloca $ \p2 ->
                                     poke p2 dst >>
                                     (withArray [n,0] $ \ft ->
                                       cvMixChannels p1 1 p2 1 ft 1))

-- |Replace a specific channel of a trichromatic image with the single
-- channel from a monochromatic image.
replaceChannel :: (HasDepth d, ImgBuilder r1, 
                   InplaceROI r2 TriChromatic d TriChromatic d) => 
                  CInt -> HIplImage MonoChromatic d r1 -> 
                  HIplImage TriChromatic d r2 -> HIplImage TriChromatic d r2
replaceChannel n c = cv2 $ \src dst -> 
                     withHIplImage c $ \cp ->
                       withArray [castPtr cp, src] $ \p1 -> 
                         withArray [dst] $ \p2 ->
                           withArray [0,n,1+n',n',1+n'',n''] $ \ft ->
                             cvMixChannels p1 2 p2 1 ft 3
  where n' = (n + 1) `rem` 3
        n'' = (n + 2) `rem` 3
