{-# LANGUAGE ForeignFunctionInterface, TypeFamilies #-}
-- |Miscellaneous image transformations.
module OpenCV.FloodFill (floodFill, FloodRange(..)) where
import Data.Bits ((.|.))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import OpenCV.Core.CxCore 
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp
import OpenCV.Core.StorableUtil

-- |Flag used to indicate whether pixels under consideration for
-- addition to a connected component should be compared to the seed
-- pixel of the component or their neighbors. Comparing to the seed
-- pixel leads to a component with a /fixed/ range, while comparing to
-- neighbors leads to a /floating/ range.
data FloodRange = FloodFixed | FloodFloating

#include <opencv2/imgproc/imgproc_c.h>

-- FIXME: This method of pushing structs onto the stack by value is
-- potentially risky as it assumes there are no packing issues
-- (e.g. bytes inserted between fields or at the end of the struct to
-- ensure a desired alignment).

#def void cvFloodFill_wrap(CvArr* img, CvPoint* seedPt,\
                           CvScalar* newVal, CvScalar* loDiff,\
                           CvScalar* upDiff, void *comp, int flags,\
                           CvArr* mask) {\
  cvFloodFill(img, *seedPt, *newVal, *loDiff, *upDiff, comp, flags, mask);\
}

-- "opencv2/imgproc/imgproc_c.h cvFloodFill"
foreign import ccall "cvFloodFill_wrap" 
  c_cvFloodFill :: Ptr CvArr -> Ptr CvPoint -> Ptr CvScalar -> Ptr CvScalar -> 
                   Ptr CvScalar -> Ptr () -> CInt -> Ptr () -> IO ()

floodHelper :: (Int, Int) -> CvScalar -> CvScalar -> CvScalar -> FloodRange -> 
               Ptr IplImage -> IO ()
floodHelper (x,y) newVal loDiff upDiff range src =
  withS (CvPoint (fromIntegral x) (fromIntegral y)) $ \seedPtr ->
  withS newVal $ \newValPtr ->
  withS loDiff $ \loDiffPtr ->
  withS upDiff $ \upDiffPtr ->
    c_cvFloodFill (castPtr src) seedPtr newValPtr loDiffPtr upDiffPtr 
                  nullPtr flags nullPtr
    -- c_cvFloodFill (castPtr src) 
    --               (CvPoint (fromIntegral x) (fromIntegral y))
    --               newVal loDiff upDiff
    --               nullPtr flags nullPtr
    where flags = case range of
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
floodFill :: (ByteOrFloat d, AsCvScalar s, s ~ CvScalarT c d) => 
             (Int, Int) -> s -> s -> s -> FloodRange -> 
             Image c d r -> Image c d r
floodFill seed newVal loDiff upDiff range img@Image{}= 
    flip cv img $ floodHelper seed (toCvScalar newVal) (toCvScalar loDiff)
                              (toCvScalar upDiff) range

{-# INLINE floodFill #-}
