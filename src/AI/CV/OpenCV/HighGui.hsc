{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
 
module AI.CV.OpenCV.HighGui where
 
import Control.Monad
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
 
import AI.CV.OpenCV.CxCore

#include <highgui.h>

------------------------------------------------
-- General
foreign import ccall unsafe "highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvConvertImage :: (IplArrayType a, IplArrayType a1) => Ptr a -> Ptr a1 -> CInt -> IO ()
cvConvertImage src dst flags = c_cvConvertImage (fromArr src) (fromArr dst) flags

------------------------------------------------
-- Capturing
data CvCapture


foreign import ccall unsafe "highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
                          
cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
cvCreateCameraCapture x = errorName "Failed to create camera" . checkPtr $ c_cvCreateCameraCapture . fromIntegral $ x
  
foreign import ccall unsafe "highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr CvCapture)
                          
cvCreateFileCapture :: String -> IO (Ptr CvCapture)
cvCreateFileCapture filename = err' . checkPtr $ withCString filename f
    where err' = errorName $ "Failed to capture from file: '" ++ filename ++ "'"
          f filenameC = c_cvCreateFileCapture filenameC
  

foreign import ccall unsafe "HOpenCV_warp.h release_capture"
  cvReleaseCapture  :: Ptr CvCapture -> IO ()

foreign import ccall unsafe "HOpenCV_warp.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr CvCapture -> IO () )
 
createCameraCaptureF :: CInt -> IO (ForeignPtr CvCapture)
createCameraCaptureF = (createForeignPtr cp_release_capture) . cvCreateCameraCapture



foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)

cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)
cvQueryFrame cap = errorName "Failed to query frame from camera" . checkPtr $ c_cvQueryFrame cap

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

foreign import ccall unsafe "highgui.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO CInt

type AutoSize = Bool

titledWindow :: String -> AutoSize -> IO Int
titledWindow s a
  = do cs <- newCString s
       i <- cvNamedWindow cs (fromToInteger $ fromEnum a)
       return $ fromToInteger i

fromToInteger :: (Integral a, Num b) => a -> b
fromToInteger = fromInteger . toInteger

newtype LoadImageColor = LoadImageColor { unLoadImageColor :: CInt }

#{enum LoadImageColor, LoadImageColor
    , loadImageColor     = CV_LOAD_IMAGE_COLOR
    , loadImageGrayscale = CV_LOAD_IMAGE_GRAYSCALE
    , loadImageUnchanged = CV_LOAD_IMAGE_UNCHANGED }

foreign import ccall unsafe "highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr IplImage)

cvLoadImage :: String -> LoadImageColor -> IO (Ptr IplImage)
cvLoadImage filename (LoadImageColor color) = err' . checkPtr $ withCString filename f
  where
    err' = errorName $ "Failed to load from file: '" ++ filename ++ "'"
    f filenameC = c_cvLoadImage filenameC color

foreign import ccall unsafe "highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr CvArr -> IO CInt

cvSaveImage :: String -> Ptr IplImage -> IO CInt
cvSaveImage filename image = withCString filename f
  where
    f filenameC = do
      ret <- c_cvSaveImage filenameC (fromArr image)
      when (ret == 0) $ fail $ "Failed to save to file: '" ++ filename ++ "'"
      return ret
