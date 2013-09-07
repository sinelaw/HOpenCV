{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module OpenCV.Core.HighGui 
    (cvLoadImage, LoadColor(..), cvSaveImage, 
     CvCapture, cvCreateCameraCapture, 
     createCameraCaptureF, createFileCaptureF,
     cvCreateFileCapture, setCapturePos, 
     CapturePos(..), cvQueryFrame,
     newWindow, delWindow, showImage, cvWaitKey,
     cvConvertImage, c_debug_ipl,
     createVideoWriterF, cvWriteFrame, FourCC, toFourCC,
     cvNamedWindow, cvDestroyWindow, cvShowImage, WindowFlag(..),
     MouseCallback, cvSetMouseCallback, wrapMouseCB, cvInit,
     windowFlagsToEnum, Event(..), EventFlag(..)) where

import Data.Bits ((.&.), (.|.), shiftL) 
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String

import Data.List (foldl') 
import OpenCV.Core.CxCore

import Foreign.Marshal.Array

#include <opencv2/highgui/highgui_c.h>

------------------------------------------------
-- General
foreign import ccall "opencv2/highgui/highgui_c.h cvConvertImage"
  c_cvConvertImage :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvConvertImage :: (IplArrayType a, IplArrayType a1) => Ptr a -> Ptr a1 -> CInt -> IO ()
cvConvertImage src dst flags = c_cvConvertImage (fromArr src) (fromArr dst) flags

-- |Determine the color model of an image loaded from a file.
data LoadColor = LoadColor     -- ^Force a 3-channel color image
               | LoadGray      -- ^Force a grayscale image
               | LoadUnchanged -- ^Load the image as is

instance Enum LoadColor where
  fromEnum LoadColor                      = (#const CV_LOAD_IMAGE_COLOR)
  fromEnum LoadGray                       = (#const CV_LOAD_IMAGE_GRAYSCALE)
  fromEnum LoadUnchanged                  = (#const CV_LOAD_IMAGE_UNCHANGED)
  toEnum (#const CV_LOAD_IMAGE_COLOR)     = LoadColor
  toEnum (#const CV_LOAD_IMAGE_GRAYSCALE) = LoadGray
  toEnum (#const CV_LOAD_IMAGE_UNCHANGED) = LoadUnchanged
  toEnum x = error $ "Unknown LoadColor enum "++show x

foreign import ccall "opencv2/highgui/highgui_c.h cvLoadImage"
  c_cvLoadImage :: CString -> CInt -> IO (Ptr IplImage)

cvLoadImage :: String -> LoadColor -> IO (Ptr IplImage)
cvLoadImage fileName col = withCString fileName (flip c_cvLoadImage col')
    where col' = fromIntegral $ fromEnum col

foreign import ccall "HOpenCV_wrap.h debug_print_image_header"
  c_debug_ipl :: Ptr IplImage -> IO ()

foreign import ccall safe "opencv2/highgui/highgui_c.h cvSaveImage"
  c_cvSaveImage :: CString -> Ptr CvArr -> Ptr Int -> IO ()

cvSaveImage :: IplArrayType a => String -> Ptr a -> IO ()
cvSaveImage fileName img = withCString fileName $
                           \str -> c_cvSaveImage str (fromArr img) nullPtr

------------------------------------------------
-- Capturing
data CvCapture


foreign import ccall "opencv2/highgui/highgui_c.h cvCreateCameraCapture"
  c_cvCreateCameraCapture :: CInt -> IO (Ptr CvCapture)
                          
cvCreateCameraCapture :: Int -> IO (Ptr CvCapture)
cvCreateCameraCapture = errorName "Failed to create camera" . checkPtr . 
                        c_cvCreateCameraCapture . fromIntegral
  
foreign import ccall "opencv2/highgui/highgui_c.h cvCreateFileCapture"
  c_cvCreateFileCapture :: CString -> IO (Ptr CvCapture)
                          
cvCreateFileCapture :: String -> IO (Ptr CvCapture)
cvCreateFileCapture filename = err' . checkPtr $ 
                               withCString filename c_cvCreateFileCapture
    where err' = errorName $ "Failed to capture from file: '" ++ filename ++ "'"

foreign import ccall "opencv2/highgui/highgui_c.h cvSetCaptureProperty"
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
  
-- foreign import ccall "HOpenCV_wrap.h release_capture"
--   release_capture  :: Ptr CvCapture -> IO ()

foreign import ccall "HOpenCV_wrap.h &release_capture"
  cp_release_capture  :: FunPtr (Ptr CvCapture -> IO ())
 
createCameraCaptureF :: Int -> IO (ForeignPtr CvCapture)
createCameraCaptureF = createForeignPtr cp_release_capture . cvCreateCameraCapture

createFileCaptureF :: String -> IO (ForeignPtr CvCapture)
createFileCaptureF = createForeignPtr cp_release_capture . cvCreateFileCapture

foreign import ccall "opencv2/highgui/highgui_c.h cvQueryFrame"
  c_cvQueryFrame :: Ptr CvCapture -> IO (Ptr IplImage)

cvQueryFrame :: Ptr CvCapture -> IO (Maybe (Ptr IplImage))
cvQueryFrame cap = ptrToMaybe `fmap` c_cvQueryFrame cap

data CvVideoWriter

type FourCC = (Char, Char, Char, Char)

-- | Parse a four-character 'String' into a 'FourCC' code (e.g. "XVID").
toFourCC :: String -> FourCC
toFourCC [a,b,c,d] = (a,b,c,d)
toFourCC c = error $ "Invalid FourCC code: "++c

fourCC :: FourCC -> CInt
fourCC (a,b,c,d) = (c1 .&. 255) + shiftL (c2 .&. 255) 8 + 
                   shiftL (c3 .&. 255) 16 + shiftL (c4 .&. 255) 24
    where [c1,c2,c3,c4] = map (fromIntegral . fromEnum) [a,b,c,d]

foreign import ccall "HOpenCV_wrap.h cvCreateVideoWriter2"
  c_cvCreateVideoWriter :: CString -> CInt -> CDouble -> CInt -> CInt -> CInt ->
                           IO (Ptr CvVideoWriter)

foreign import ccall "HOpenCV_wrap.h &release_video_writer"
  cp_release_writer :: FunPtr (Ptr CvVideoWriter -> IO ())

cvCreateVideoWriter :: FilePath -> FourCC -> Double -> (Int, Int) -> 
                       IO (Ptr CvVideoWriter)
cvCreateVideoWriter fname codec fps (w,h) = do
    withCString fname $ \str ->
        c_cvCreateVideoWriter str (fourCC codec) (realToFrac fps) 
            (fromIntegral w) (fromIntegral h) 1

-- |Create a video file writer.
createVideoWriterF :: FilePath -> FourCC -> Double -> (Int, Int) -> 
                      IO (ForeignPtr CvVideoWriter)
createVideoWriterF fname codec fps sz =
    createForeignPtr cp_release_writer $ cvCreateVideoWriter fname codec fps sz

foreign import ccall "opencv2/highgui/highgui_c.h cvWriteFrame"
  cvWriteFrame :: Ptr CvVideoWriter -> Ptr IplImage -> IO ()

{-
foreign import ccall "opencv2/core/types_c.h cvSize"
  cvSize :: Int -> Int -> IO (Ptr ())
-}

-------------------------------------------------
-- Windows
foreign import ccall "HOpenCV_wrap.h new_window"
  c_newWindow :: CInt -> CInt -> IO ()

newWindow :: CInt -> Bool -> IO ()
newWindow num autoSize = c_newWindow num (if autoSize then 1 else 0)

foreign import ccall "HOpenCV_wrap.h del_window"
  delWindow :: CInt -> IO ()

foreign import ccall "HOpenCV_wrap.h show_image"
  showImage :: CInt -> Ptr IplImage -> IO ()

foreign import ccall "opencv2/highgui/highgui_c.h cvWaitKey"
  cvWaitKey :: CInt -> IO CInt

-- New Windowing Code

foreign import ccall "opencv2/highgui/highgui_c.h cvInitSystem"
  cvInitSystem :: CInt -> Ptr CString -> IO ()

cvInit :: IO ()
cvInit = cvInitSystem 0 nullPtr

foreign import ccall "opencv2/highgui/highgui_c.h cvNamedWindow"
  cvNamedWindow :: CString -> CInt -> IO ()

foreign import ccall "opencv2/highgui/highgui_c.h cvDestroyWindow"
  cvDestroyWindow :: CString -> IO ()

foreign import ccall "opencv2/highgui/highgui_c.h cvShowImage"
  cvShowImage :: CString -> Ptr CvArr -> IO ()

type CMouseCallback = CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()

foreign import ccall "opencv2/highgui/highgui_c.h cvSetMouseCallback"
  cvSetMouseCallback :: CString -> FunPtr CMouseCallback -> Ptr () -> IO ()

foreign import ccall "wrapper"
  mkMouseCB :: CMouseCallback -> IO (FunPtr CMouseCallback)

data WindowFlag = AutoSize

windowFlagToEnum :: WindowFlag -> CInt
windowFlagToEnum AutoSize = #{const CV_WINDOW_AUTOSIZE}

windowFlagsToEnum :: [WindowFlag] -> CInt
windowFlagsToEnum = foldl' (.|.) 0 . map windowFlagToEnum

data Event = MouseMove | LButtonDown | RButtonDown | MButtonDown
           | LButtonUp | RButtonUp | MButtonUp | LButtonDblClk
           | RButtonDblClk | MButtonDblClk

type MouseCallback = Event -> (Int,Int) -> [EventFlag] -> IO ()

wrapMouseCB :: MouseCallback -> IO (FunPtr CMouseCallback)
wrapMouseCB cb = mkMouseCB $ 
                 \e x y f _ -> cb (enumToEvent e) 
                                  (fromIntegral x, fromIntegral y)
                                  (enumToEventFlags f)
                                  
enumToEvent :: CInt -> Event
enumToEvent (#const CV_EVENT_MOUSEMOVE)     = MouseMove
enumToEvent (#const CV_EVENT_LBUTTONDOWN)   = LButtonDown
enumToEvent (#const CV_EVENT_RBUTTONDOWN)   = RButtonDown
enumToEvent (#const CV_EVENT_MBUTTONDOWN)   = MButtonDown
enumToEvent (#const CV_EVENT_LBUTTONUP)     = LButtonUp
enumToEvent (#const CV_EVENT_RBUTTONUP)     = RButtonUp
enumToEvent (#const CV_EVENT_MBUTTONUP)     = MButtonUp
enumToEvent (#const CV_EVENT_LBUTTONDBLCLK) = LButtonDblClk
enumToEvent (#const CV_EVENT_RBUTTONDBLCLK) = RButtonDblClk
enumToEvent (#const CV_EVENT_MBUTTONDBLCLK) = MButtonDblClk
enumToEvent x                               = error $ "Unkonwn event "++show x

data EventFlag = LButton | RButton | MButton | CtrlKey | ShiftKey | AltKey
                 deriving (Enum, Bounded)

eventFlagToEnum :: EventFlag -> CInt
eventFlagToEnum LButton  = (#const CV_EVENT_FLAG_LBUTTON)
eventFlagToEnum RButton  = (#const CV_EVENT_FLAG_RBUTTON)
eventFlagToEnum MButton  = (#const CV_EVENT_FLAG_MBUTTON)
eventFlagToEnum CtrlKey  = (#const CV_EVENT_FLAG_CTRLKEY)
eventFlagToEnum ShiftKey = (#const CV_EVENT_FLAG_SHIFTKEY)
eventFlagToEnum AltKey   = (#const CV_EVENT_FLAG_ALTKEY)

enumToEventFlags :: CInt -> [EventFlag]
enumToEventFlags x = map fst . filter snd $
                     zip [minBound..maxBound]
                         (map ((> 0) . (x .|.)) 
                              (map eventFlagToEnum [minBound..maxBound]))

-- Qt fonts
-- foreign import ccall "opencv2/highgui/highgui_c.h cvFontQt"
--   cvFontQt :: CString -> CInt -> CDouble -> CDouble -> CDouble -> 
--               CInt -> CInt -> CInt -> IO CvFont
