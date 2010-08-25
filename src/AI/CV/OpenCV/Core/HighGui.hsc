{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module AI.CV.OpenCV.Core.HighGui 
    (cvLoadImage, LoadColor(..), cvSaveImage, 
     CvCapture, cvCreateCameraCapture, 
     createCameraCaptureF, createFileCaptureF,
     cvCreateFileCapture, setCapturePos, 
     CapturePos(..), cvQueryFrame,
     newWindow, delWindow, showImage, waitKey,
     cvConvertImage, c_debug_ipl,
     createVideoWriterF, cvWriteFrame, FourCC) where

import Data.Bits ((.&.), shiftL) 
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
 
import AI.CV.OpenCV.Core.CxCore

#include <opencv/highgui.h>

------------------------------------------------
-- General
foreign import ccall unsafe "highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvConvertImage :: (IplArrayType a, IplArrayType a1) => Ptr a -> Ptr a1 -> CInt -> IO ()
cvConvertImage src dst flags = c_cvConvertImage (fromArr src) (fromArr dst) flags

-- |Determine the color model of an image loaded from a file.
data LoadColor = LoadColor     -- ^Force a 3-channel color image
               | LoadGray      -- ^Force a grayscale image
               | LoadUnchanged -- ^Load the image as is
                 deriving Enum

foreign import ccall unsafe "highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr IplImage)

cvLoadImage :: String -> LoadColor -> IO (Ptr IplImage)
cvLoadImage fileName col = withCString fileName $ 
                           \str -> c_cvLoadImage str col'
    where col' = fromIntegral $ fromEnum col

foreign import ccall unsafe "HOpenCV_wrap.h debug_print_image_header"
  c_debug_ipl :: Ptr IplImage -> IO ()

foreign import ccall safe "highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr CvArr -> Ptr Int -> IO ()

cvSaveImage :: IplArrayType a => String -> Ptr a -> IO ()
cvSaveImage fileName img = withCString fileName $
                           \str -> c_cvSaveImage str (fromArr img) nullPtr

------------------------------------------------
-- Capturing
data CvCapture


foreign import ccall unsafe "highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
                          
cvCreateCameraCapture :: Int -> IO (Ptr CvCapture)
cvCreateCameraCapture = errorName "Failed to create camera" . checkPtr . 
                        c_cvCreateCameraCapture . fromIntegral
  
foreign import ccall unsafe "highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr CvCapture)
                          
cvCreateFileCapture :: String -> IO (Ptr CvCapture)
cvCreateFileCapture filename = err' . checkPtr $ 
                               withCString filename c_cvCreateFileCapture
    where err' = errorName $ "Failed to capture from file: '" ++ filename ++ "'"

foreign import ccall unsafe "highgui.h cvSetCaptureProperty"
  c_cvSetCaptureProperty :: Ptr CvCapture -> CInt -> CDouble -> IO ()

-- |The current position of a video capture.
data CapturePos = PosMsec Double  
                -- ^Position in milliseconds or video timestamp.
                | PosFrames Int   
                -- ^0-based index of the next frame to be decoded or captured.
                | PosRatio Double 
                -- ^Relative position of the video file (0 = start, 1 = end).

posEnum :: CapturePos -> (CInt, CDouble)
posEnum (PosMsec t) = (#{const CV_CAP_PROP_POS_MSEC}, realToFrac t)
posEnum (PosFrames n) = (#{const CV_CAP_PROP_POS_FRAMES}, fromIntegral n)
posEnum (PosRatio r) = (#{const CV_CAP_PROP_POS_AVI_RATIO}, realToFrac r)

-- |Set the current position of a video capture.
setCapturePos :: Ptr CvCapture -> CapturePos -> IO ()
setCapturePos cap pos = uncurry (c_cvSetCaptureProperty cap) $ posEnum pos
  
-- foreign import ccall unsafe "HOpenCV_wrap.h release_capture"
--   release_capture  :: Ptr CvCapture -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr CvCapture -> IO ())
 
createCameraCaptureF :: Int -> IO (ForeignPtr CvCapture)
createCameraCaptureF = createForeignPtr cp_release_capture . cvCreateCameraCapture

createFileCaptureF :: String -> IO (ForeignPtr CvCapture)
createFileCaptureF = createForeignPtr cp_release_capture . cvCreateFileCapture

foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)

cvQueryFrame :: Ptr CvCapture -> IO (Maybe (Ptr IplImage))
cvQueryFrame cap = ptrToMaybe `fmap` c_cvQueryFrame cap

data CvVideoWriter

type FourCC = (Char, Char, Char, Char)

fourCC :: FourCC -> CInt
fourCC (a,b,c,d) = (c1 .&. 255) + shiftL (c2 .&. 255) 8 + 
                   shiftL (c3 .&. 255) 16 + shiftL (c4 .&. 255) 24
    where [c1,c2,c3,c4] = map (fromIntegral . fromEnum) [a,b,c,d]

foreign import ccall unsafe "highgui.h cvCreateVideoWriter"
  c_cvCreateVideoWriter :: CString -> CInt -> CDouble -> CInt -> CInt -> CInt ->
                           IO (Ptr CvVideoWriter)

foreign import ccall unsafe "HOpenCV_wrap.h &release_video_writer"
  cp_release_writer :: FunPtr (Ptr CvVideoWriter -> IO ())

cvCreateVideoWriter :: FilePath -> FourCC -> Double -> (Int, Int) -> 
                       IO (Ptr CvVideoWriter)
cvCreateVideoWriter fname codec fps (w, h) = 
    withCString fname $ \str ->
        c_cvCreateVideoWriter str (fourCC codec) (realToFrac fps) 
                              (fromIntegral w) (fromIntegral h) 1
                              
-- |Create a video file writer.
createVideoWriterF :: FilePath -> FourCC -> Double -> (Int, Int) -> 
                      IO (ForeignPtr CvVideoWriter)
createVideoWriterF fname codec fps sz = createForeignPtr cp_release_writer $
                                        cvCreateVideoWriter fname codec fps sz

foreign import ccall unsafe "highgui.h cvWriteFrame"
  cvWriteFrame :: Ptr CvVideoWriter -> Ptr IplImage -> IO ()

-------------------------------------------------
-- Windows
foreign import ccall unsafe "HOpenCV_wrap.h new_window"
  c_newWindow :: CInt -> CInt -> IO ()

newWindow :: CInt -> Bool -> IO ()
newWindow num autoSize = c_newWindow num (if autoSize then 1 else 0)

foreign import ccall unsafe "HOpenCV_wrap.h del_window"
  delWindow :: CInt -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h show_image"
  showImage :: CInt -> Ptr IplImage -> IO ()

foreign import ccall unsafe "highgui.h cvWaitKey"
  waitKey :: CInt -> IO CInt
