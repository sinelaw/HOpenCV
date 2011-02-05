{-# LANGUAGE ForeignFunctionInterface #-}
-- |Motion analysis functions. Possibly BROKEN in OpenCV 2.2.
module AI.CV.OpenCV.Motion (calcOpticalFlowBM) where
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplImage

-- FIXME: This is missing from the C API of OpenCV 2.2
--foreign import ccall unsafe "opencv/cv.h cvCalcOpticalFlowBM"
foreign import ccall unsafe "opencv2/video/tracking.hpp cvCalcOpticalFlowBM"
  c_cvCalcOpticalFlowBM :: Ptr CvArr -> Ptr CvArr -> CInt -> CInt -> 
                           CInt -> CInt -> CInt -> CInt -> 
                           CInt -> Ptr CvArr -> Ptr CvArr -> IO ()

-- |Calculates the optical flow between two images using the block
-- matching method. The third parameter is the width and height of the
-- blocks to be compared; the fourth parameter is the block coordinate
-- increments; the fifth is the size of the scanned neighborhood in
-- pixels around the block. The result is a pair of the horizontal and
-- vertical components of optical flow.
calcOpticalFlowBM :: HIplImage MonoChromatic Word8 -> 
                     HIplImage MonoChromatic Word8 -> 
                     (Int,Int) -> (Int,Int) -> (Int,Int) -> 
                     (HIplImage MonoChromatic Float, 
                      HIplImage MonoChromatic Float)
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
    where w = (width prev - fst blockSize) `div` fst shiftSize
          h = (height prev - snd blockSize) `div` snd shiftSize
          sw = fromIntegral . fst -- size width
          sh = fromIntegral . snd -- size height
