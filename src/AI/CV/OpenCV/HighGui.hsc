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
import AI.CV.OpenCV.Util

#include <highgui.h>

------------------------------------------------
-- General
foreign import ccall unsafe "highgui.h cvConvertImage"
  c_cvConvertImage :: Ptr Priv_IplImage -> Ptr Priv_IplImage -> CInt -> IO ()

convertImage :: IplImage -> IplImage -> Int -> IO ()
convertImage src dst flags
  = withForeignPtr2 src dst
     $ \s d -> c_cvConvertImage s d
                                (fromIntegral flags)

------------------------------------------------
-- Capturing
data Priv_CvCapture
type Capture = ForeignPtr Priv_CvCapture


foreign import ccall unsafe "highgui.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr Priv_CvCapture)
                          
-- | self-documenting camera specification
pickAnyCam :: Int
pickAnyCam = -1

-- | self-documenting camera specification
cam :: Int -> Int
cam = id

createCameraCapture :: Int -> IO Capture
createCameraCapture x
  = do p <- errorName "Failed to create camera" . checkPtr $ c_cvCreateCameraCapture . fromIntegral $ x
       newForeignPtr cp_release_capture p
  
foreign import ccall unsafe "highgui.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr Priv_CvCapture)
                          
createFileCapture :: String -> IO Capture
createFileCapture filename
  = do c <- err' . checkPtr $ withCString filename f
       newForeignPtr cp_release_capture c
    where err' = errorName $ "Failed to capture from file: '" ++ filename ++ "'"
          f filenameC = c_cvCreateFileCapture filenameC
  

foreign import ccall unsafe "HOpenCV_wrap.h release_capture"
  cvReleaseCapture  :: Ptr Priv_CvCapture -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr Priv_CvCapture -> IO ())
 
foreign import ccall unsafe "highgui.h cvQueryFrame"
  c_cvQueryFrame :: Ptr Priv_CvCapture -> IO (Ptr Priv_IplImage)

queryFrame :: Capture -> IO IplImage
queryFrame cap
  = do i <- withForeignPtr cap $ \c ->
              errorName "Failed to query frame from camera" . checkPtr
              $ c_cvQueryFrame c
       fp <- newForeignPtr cvFree i
       return fp

-------------------------------------------------
-- Windows

foreign import ccall unsafe "highgui.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO CInt

type AutoSize = Bool

-- | self-documenting window sizing specification
autoSize :: AutoSize
autoSize   = True

namedWindow :: String -> AutoSize -> IO ()
namedWindow s a
  = withCString s $ \cs ->
      do _ <- cvNamedWindow cs (fromIntegral $ fromEnum a)
         return ()

foreign import ccall unsafe "highgui.h cvDestroyWindow"
  cvDestroyWindow :: CString -> IO ()

destroyWindow :: String -> IO ()
destroyWindow wId
  = withCString wId cvDestroyWindow 

foreign import ccall unsafe "highgui.h cvShowImage"
  cvShowImage :: CString -> Ptr Priv_IplImage -> IO ()

showImage :: String -> IplImage -> IO ()
showImage wId p
 = withCString wId $ \w ->
    withForeignPtr p $ cvShowImage w

foreign import ccall unsafe "highgui.h cvWaitKey"
  cvWaitKey :: CInt -> IO CInt

waitKey :: Int -> IO (Maybe Int)
waitKey milliSecs
  = do i <- cvWaitKey $ fromIntegral milliSecs
       if i == (-1)
         then return Nothing
         else return $ Just $ fromIntegral i

newtype LoadImageColor = LoadImageColor { unLoadImageColor :: CInt }

#{enum LoadImageColor, LoadImageColor
    , loadImageColor     = CV_LOAD_IMAGE_COLOR
    , loadImageGrayscale = CV_LOAD_IMAGE_GRAYSCALE
    , loadImageUnchanged = CV_LOAD_IMAGE_UNCHANGED }

foreign import ccall unsafe "highgui.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr Priv_IplImage)

loadImage :: String -> LoadImageColor -> IO IplImage
loadImage filename (LoadImageColor color)
  = do i <- err' . checkPtr $ withCString filename 
            $ \fn -> c_cvLoadImage fn color
       fp <- newForeignPtr cvFree i
       return fp
 where
   err' = errorName $ "Failed to load from file: '" ++ filename ++ "'"

foreign import ccall unsafe "highgui.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr Priv_IplImage -> IO CInt

saveImage :: String -> IplImage -> IO Int
saveImage filename image = withCString filename f
  where
    f filenameC = do
      ret <- withForeignPtr image $ \i ->
             c_cvSaveImage filenameC i
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
