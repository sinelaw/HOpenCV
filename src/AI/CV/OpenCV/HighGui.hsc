{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
 
module AI.CV.OpenCV.HighGui where
 
import Control.Monad
import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
 
import AI.CV.OpenCV.CxCore

#include <highgui.h>

------------------------------------------------
-- General
foreign import ccall unsafe "highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CInt -> IO ()

cvConvertImage :: (IplArrayType a, IplArrayType a1) => a -> a1 -> Int -> IO ()
cvConvertImage src dst flags
  = let CvArr src' = fromArr src
        CvArr dst' = fromArr dst
    in c_cvConvertImage src' dst' (fromIntegral flags)

------------------------------------------------
-- Capturing
data Priv_CvCapture
type CvCapture  = Ptr        Priv_CvCapture
type CvCaptureF = ForeignPtr Priv_CvCapture


foreign import ccall unsafe "highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr Priv_CvCapture)
                          
cvCreateCameraCapture :: Int -> IO CvCapture
cvCreateCameraCapture x
  = errorName "Failed to create camera" . checkPtr $ c_cvCreateCameraCapture . fromIntegral $ x
  
foreign import ccall unsafe "highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr Priv_CvCapture)
                          
cvCreateFileCapture :: String -> IO CvCapture
cvCreateFileCapture filename = err' . checkPtr $ withCString filename f
    where err' = errorName $ "Failed to capture from file: '" ++ filename ++ "'"
          f filenameC = c_cvCreateFileCapture filenameC
  

foreign import ccall unsafe "HOpenCV_wrap.h release_capture"
  cvReleaseCapture  :: Ptr Priv_CvCapture -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr Priv_CvCapture -> IO ())
 
createCameraCaptureF :: Int -> IO CvCaptureF
createCameraCaptureF
  = createForeignPtr cp_release_capture
  . cvCreateCameraCapture
  . fromIntegral

foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr Priv_CvCapture -> IO (Ptr Priv_IplImage)

cvQueryFrame :: CvCapture -> IO IplImage
cvQueryFrame cap
  = do i <- errorName "Failed to query frame from camera" . checkPtr
            $ c_cvQueryFrame cap
       return $ IplImage i

-------------------------------------------------
-- Windows
foreign import ccall unsafe "HOpenCV_wrap.h new_window"
  c_newWindow :: CInt -> CInt -> IO ()

newWindow :: Int -> Bool -> IO ()
newWindow num autoSiz
  = c_newWindow (fromIntegral num) (if autoSiz then 1 else 0)

foreign import ccall unsafe "HOpenCV_wrap.h del_window"
  delWindow :: CInt -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h show_image"
  cvShowImage :: CInt -> Ptr Priv_IplImage -> IO ()

showImage :: Int -> IplImage -> IO ()
showImage i (IplImage p)
 = cvShowImage (fromIntegral i) p

foreign import ccall unsafe "highgui.h cvWaitKey"
  cvWaitKey :: CInt -> IO CInt

waitKey :: Int -> IO Int
waitKey milliSecs
  = do i <- cvWaitKey $ fromIntegral milliSecs
       return $ fromIntegral i

foreign import ccall unsafe "highgui.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO CInt

type AutoSize = Bool
autoSize :: AutoSize
autoSize   = True

namedWindow :: String -> AutoSize -> IO Int
namedWindow s a
  = withCString s $ \cs ->
      do i <- cvNamedWindow cs (fromIntegral $ fromEnum a)
         return $ fromIntegral i

newtype LoadImageColor = LoadImageColor { unLoadImageColor :: CInt }

#{enum LoadImageColor, LoadImageColor
    , loadImageColor     = CV_LOAD_IMAGE_COLOR
    , loadImageGrayscale = CV_LOAD_IMAGE_GRAYSCALE
    , loadImageUnchanged = CV_LOAD_IMAGE_UNCHANGED }

foreign import ccall unsafe "highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr Priv_IplImage)

cvLoadImage :: String -> LoadImageColor -> IO IplImage
cvLoadImage filename (LoadImageColor color)
  = do i <- err' . checkPtr $ withCString filename 
            $ \fn -> c_cvLoadImage fn color
       return $ IplImage i
 where
   err' = errorName $ "Failed to load from file: '" ++ filename ++ "'"

foreign import ccall unsafe "highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr Priv_CvArr -> IO CInt

cvSaveImage :: String -> IplImage -> IO Int
cvSaveImage filename image = withCString filename f
  where
    f filenameC = do
      let CvArr i = fromArr image
      ret <- c_cvSaveImage filenameC i
      when (ret == 0) $ fail $ "Failed to save to file: '" ++ filename ++ "'"
      return $ fromIntegral ret

------------------------------------------------
-- Trackbar

foreign import ccall unsafe "HOpenCV_Wrap.h wrap_createTrackbar"
  wrap_createTrackbar :: CString -> CString -> Ptr CInt -> CInt -> IO ()

createTrackbar :: String -> String -> Maybe Int -> Int -> IO ()
createTrackbar trackbarName winName startPosition maxValue
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
    alloca                   $ \sp -> 
      do maybeToPtr sp startPosition
         wrap_createTrackbar tb wn sp (fromIntegral maxValue)
 where
  maybeToPtr mem (Just i) = poke mem (fromIntegral i)
  maybeToPtr mem Nothing  = poke mem (fromIntegral 0)

foreign import ccall unsafe "highgui.h cvGetTrackbarPos"
  cvGetTrackbarPos :: CString -> CString -> IO CInt

getTrackbarPos :: String -> String -> IO Int
getTrackbarPos trackbarName winName
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      do i <- cvGetTrackbarPos tb wn
         return $ fromIntegral i

foreign import ccall unsafe "highgui.h cvSetTrackbarPos"
  cvSetTrackbarPos :: CString -> CString -> CInt -> IO ()

setTrackbarPos :: String -> String -> Int -> IO ()
setTrackbarPos trackbarName winName pos
  = withCString trackbarName $ \tb ->
    withCString winName      $ \wn ->
      cvSetTrackbarPos tb wn (fromIntegral pos)
