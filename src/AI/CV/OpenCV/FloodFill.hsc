{-# LANGUAGE ForeignFunctionInterface, TypeFamilies #-}
-- |Miscellaneous image transformations.
module AI.CV.OpenCV.FloodFill (floodFill, FloodRange(..)) where
import Data.Bits ((.|.))
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import AI.CV.OpenCV.Core.CxCore 
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp

-- |Flag used to indicate whether pixels under consideration for
-- addition to a connected component should be compared to the seed
-- pixel of the component or their neighbors. Comparing to the seed
-- pixel leads to a component with a /fixed/ range, while comparing to
-- neighbors leads to a /floating/ range.
data FloodRange = FloodFixed | FloodFloating

#include <opencv2/imgproc/imgproc_c.h>

foreign import ccall unsafe "opencv2/imgproc/imgproc_c.h cvFloodFill"
  c_cvFloodFill :: Ptr CvArr -> CInt -> CInt -> 
                   CDouble -> CDouble -> CDouble -> CDouble ->
                   CDouble -> CDouble -> CDouble -> CDouble ->
                   CDouble -> CDouble -> CDouble -> CDouble ->
                   Ptr () -> CInt -> Ptr () -> IO ()

type CvD = (CDouble, CDouble, CDouble, CDouble)

floodHelper :: (Int, Int) -> CvD -> CvD -> CvD -> FloodRange -> 
               Ptr IplImage -> IO ()
floodHelper (x,y) newVal loDiff upDiff range src =
    c_cvFloodFill (castPtr src) (fromIntegral x) (fromIntegral y) 
                  nv1 nv2 nv3 nv4 lo1 lo2 lo3 lo4 up1 up2 up3 up4 
                  nullPtr flags nullPtr
    where (nv1,nv2,nv3,nv4) = newVal
          (lo1,lo2,lo3,lo4) = loDiff
          (up1,up2,up3,up4) = upDiff
          flags = case range of
                    FloodFixed -> 4 .|. #{const CV_FLOODFILL_FIXED_RANGE}
                    FloodFloating -> 4

-- |Fills a connected component with the given color. Parameters are
-- the starting point (x,y) coordinates; the new value of the
-- repainted pixels; the maximal lower brigtness/color difference
-- between the currently observed pixel and one of its neighbors
-- belonging to the component, or the seed pixel; the maximal upper
-- brightness/color difference between the currently observed pixel
-- and one of its neighbors belonging to teh component, or the seed
-- pixel; a flag indicating whether pixels under consideration for
-- painting should be compared to the seed pixel ('FloodFixed') or to
-- their neighbors ('FloodFloating'); the source image.
{-
floodFill :: (ByteOrFloat d, HasChannels c, HasScalar c d, 
              IsCvScalar s, s ~ CvScalar c d) => 
             (Int, Int) -> s -> s -> s -> FloodRange -> HIplImage c d -> 
             HIplImage c d
floodFill seed newVal loDiff upDiff range src = 
    fst . withDuplicateImage src $ \ptr ->
        floodHelper seed (toCvScalar newVal) (toCvScalar loDiff) 
                    (toCvScalar upDiff) range ptr

unsafeFlood :: (ByteOrFloat d, HasChannels c, HasScalar c d, 
                IsCvScalar s, s ~ CvScalar c d) => 
               (Int, Int) -> s -> s -> s -> FloodRange -> HIplImage c d -> 
               IO (HIplImage c d)
unsafeFlood seed newVal loDiff upDiff range src = 
    withHIplImage src $ \ptr ->
        do floodHelper seed (toCvScalar newVal) (toCvScalar loDiff) 
                       (toCvScalar upDiff) range ptr
           return src
-}
floodFill :: (ByteOrFloat d, HasChannels c, HasScalar c d, 
              IsCvScalar s, s ~ CvScalar c d) => 
             (Int, Int) -> s -> s -> s -> FloodRange -> HIplImage c d -> 
             HIplImage c d
floodFill seed newVal loDiff upDiff range = 
    cv $ floodHelper seed (toCvScalar newVal) (toCvScalar loDiff)
                     (toCvScalar upDiff) range

{-# INLINE [1] floodFill #-}
