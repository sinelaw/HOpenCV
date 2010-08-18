{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
 
module AI.CV.OpenCV.HighGui where
 
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
 
import AI.CV.OpenCV.CxCore


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
                          
cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
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

resetCapturePos :: Ptr CvCapture -> IO ()
resetCapturePos cap = c_cvSetCaptureProperty cap 0 0

cvQueryFrame2 :: Ptr CvCapture -> IO (Ptr IplImage)
cvQueryFrame2 cap = do frame <- c_cvQueryFrame cap
                       if frame == nullPtr
                          then resetCapturePos cap >> cvQueryFrame cap
                          else return frame
  
foreign import ccall unsafe "HOpenCV_wrap.h release_capture"
  release_capture  :: Ptr CvCapture -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr CvCapture -> IO ())
 
createCameraCaptureF :: CInt -> IO (ForeignPtr CvCapture)
createCameraCaptureF = createForeignPtr cp_release_capture . cvCreateCameraCapture

createFileCaptureF :: String -> IO (ForeignPtr CvCapture)
createFileCaptureF = createForeignPtr cp_release_capture . cvCreateFileCapture

foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)

cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)
cvQueryFrame cap = errorName "Failed to query frame from capture device" . 
                   checkPtr $ c_cvQueryFrame cap

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
