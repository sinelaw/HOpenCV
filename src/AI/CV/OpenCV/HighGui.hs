{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
 
module AI.CV.OpenCV.HighGui where
 
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
 
import AI.CV.OpenCV.CxCore


------------------------------------------------
-- Capturing
data CvCapture

foreign import ccall unsafe "highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
                          
foreign import ccall unsafe "HOpenCV_warp.h &release_capture"
  c_release_capture  :: FunPtr (Ptr CvCapture -> IO () )
 
c_createCameraCapture :: CInt -> IO (Maybe (ForeignPtr CvCapture) )
c_createCameraCapture = createForeignPtr c_cvCreateCameraCapture c_release_capture

createCameraCapture :: Integral a => a -> IO (Maybe (ForeignPtr CvCapture))
createCameraCapture = c_createCameraCapture . fromIntegral

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
  