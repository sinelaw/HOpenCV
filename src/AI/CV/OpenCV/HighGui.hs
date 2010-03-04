{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
 
module AI.CV.OpenCV.HighGui where
 
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
 
import AI.CV.OpenCV.CxCore


------------------------------------------------
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
                          
foreign import ccall unsafe "HOpenCV_warp.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr CvCapture -> IO () )
 
c_createCameraCapture :: CInt -> IO (Maybe (ForeignPtr CvCapture) )
c_createCameraCapture = (createForeignPtr cp_release_capture) . c_cvCreateCameraCapture

createCameraCaptureF :: Integral a => a -> IO (Maybe (ForeignPtr CvCapture))
createCameraCaptureF = c_createCameraCapture . fromIntegral

foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)


toMaybe :: (Eq a) => a -> a -> Maybe a
toMaybe nValue x = if x == nValue then Nothing else Just x

cvQueryFrame :: Ptr CvCapture -> IO (Maybe (Ptr IplImage))
cvQueryFrame cap = do
  frame <- c_cvQueryFrame cap
  return $ toMaybe nullPtr frame

-------------------------------------------------
-- Windows
foreign import ccall unsafe "HOpenCV_wrap.h new_window"
  c_new_window :: CInt -> CInt -> IO ()

newWindow :: Integral a => a -> IO ()
newWindow num = c_new_window (fromIntegral num) 1
  

foreign import ccall unsafe "HOpenCV_wrap.h del_window"
  c_del_window :: CInt -> IO ()

delWindow :: Integral a => a -> IO ()
delWindow = c_del_window . fromIntegral


foreign import ccall unsafe "HOpenCV_wrap.h show_image"
  c_show_image :: CInt -> Ptr IplImage -> IO ()

showImage :: Integral a => a -> Ptr IplImage -> IO ()
showImage = c_show_image . fromIntegral

foreign import ccall unsafe "highgui.h cvWaitKey"
  c_waitKey :: CInt -> IO CInt
               
waitKey :: (Integral a, Integral b) => a -> IO b
waitKey = f . fromIntegral 
    where f x = do
            key <- c_waitKey x
            return $ fromIntegral key
  