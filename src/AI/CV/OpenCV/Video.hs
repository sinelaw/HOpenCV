{-# Language ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.Video
where

import Foreign
import Foreign.C
import AI.CV.OpenCV.CxCore

data Priv_CvVideoWriter

type CvVideoWriter = Ptr Priv_CvVideoWriter

foreign import ccall unsafe "HOpenCV_wrap.h wrap_cvCreateVideoWriter"
  wrap_cvCreateVideoWriter :: CString -> CInt -> CDouble -> CInt -> CInt -> IO (Ptr Priv_CvVideoWriter)

type FourCC = String

createVideoWriter :: String -> FourCC -> Double -> CvSize -> IO CvVideoWriter
createVideoWriter file fourCC fps size
  = withCString file $ \f  ->
    wrap_cvCreateVideoWriter f (toCInt fourCC) (realToFrac fps) (sizeWidth size) (sizeHeight size)
 where
  toCInt = fromIntegral . sum . map fromEnum

foreign import ccall unsafe "highgui.h cvReleaseVideoWriter"
  releaseVideoWriter :: CvVideoWriter -> IO ()

foreign import ccall unsafe "highgui.h cvWriteFrame"
  cvWriteFrame :: CvVideoWriter -> Ptr IplImage -> IO CInt

writeFrame :: CvVideoWriter -> Ptr IplImage -> IO Int
writeFrame vw im
  = do i <- cvWriteFrame vw im
       return $ fromIntegral i
