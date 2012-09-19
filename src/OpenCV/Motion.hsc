{-# LANGUAGE ForeignFunctionInterface #-}
-- |Motion analysis functions.
module OpenCV.Motion (calcOpticalFlowBM) where
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe
import OpenCV.Core.CxCore
import OpenCV.Core.HIplImage

foreign import ccall "opencv2/video/tracking.hpp cvCalcOpticalFlowBM"
  c_cvCalcOpticalFlowBM :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> 
                           CInt -> CInt -> CInt -> CInt -> 
                           CInt -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculates the optical flow between two images using the block
-- matching method. The third parameter is the width and height of the
-- blocks to be compared; the fourth parameter is the block coordinate
-- increments; the fifth is the size of the scanned neighborhood in
-- pixels around the block. The result is a pair of the horizontal and
-- vertical components of optical flow.
calcOpticalFlowBM :: HIplImage Monochromatic Word8 r -> 
                     HIplImage Monochromatic Word8 r -> 
                     (Int,Int) -> (Int,Int) -> (Int,Int) -> 
                     (HIplImage Monochromatic Float NoROI, 
                      HIplImage Monochromatic Float NoROI)
calcOpticalFlowBM prev curr blockSize shiftSize maxRange = 
    unsafePerformIO $
    do velX <- mkHIplImage w h 
       velY <- mkHIplImage w h
       withHIplImage prev $ \prevPtr ->
         withHIplImage curr $ \currPtr ->
           withHIplImage velX $ \vxPtr ->
             withHIplImage velY $ \vyPtr -> 
                  c_cvCalcOpticalFlowBM (fromArr prevPtr) (fromArr currPtr) 
                                        (sw blockSize) (sh blockSize)
                                        (sw shiftSize) (sh shiftSize)
                                        (sw maxRange) (sh maxRange)
                                        0 (fromArr vxPtr) (fromArr vyPtr)
       return (velX, velY)
    where fi = fromIntegral
          w = (fi (width prev) - fst blockSize) `div` fst shiftSize
          h = (fi (height prev) - snd blockSize) `div` snd shiftSize
          sw = fromIntegral . fst -- size width
          sh = fromIntegral . snd -- size height
{-# NOINLINE calcOpticalFlowBM #-}