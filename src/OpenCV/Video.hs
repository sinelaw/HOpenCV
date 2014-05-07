{-# LANGUAGE DataKinds #-}
-- |Interfaces for grabbing images from cameras and video files, and
-- for writing to video files.
module OpenCV.Video (createFileCapture, createFileCaptureLoop, 
                     createCameraCapture, createVideoWriter, 
                     FourCC, toFourCC, mpeg4CC) where
import Data.Maybe (fromMaybe)
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import OpenCV.Core.HighGui

-- |Raise an error if 'cvQueryFrame' returns 'Nothing'; otherwise
-- returns a 'Ptr' 'IplImage'.
queryError :: Ptr CvCapture -> IO (Ptr IplImage)
queryError = fmap (fromMaybe $ error "Unable to capture frame") . cvQueryFrame

-- |If 'cvQueryFrame' returns 'Nothing', try rewinding the video and
-- querying again. If it still fails, raise an error. When a non-null
-- frame is obtained, return it.
queryFrameLoop :: Ptr CvCapture -> IO (Ptr IplImage)
queryFrameLoop cap = do f <- cvQueryFrame cap
                        case f of
                          Nothing -> do setCapturePos cap (PosFrames 0)
                                        queryError cap
                          Just f' -> return f'

-- |Open a capture stream from a movie file. The returned action may
-- be used to query for the next available frame. If no frame is
-- available either due to error or the end of the video sequence,
-- 'Nothing' is returned.
createFileCapture :: (HasDepth d, SingI c) => 
                     FilePath -> IO (IO (Maybe (Image c d NoROI)))
createFileCapture fname = do capture <- createFileCaptureF fname
                             return (withForeignPtr capture $ \cap ->
                                       do f <- cvQueryFrame cap
                                          case f of
                                            Nothing -> return Nothing
                                            Just f' -> Just `fmap` peekIpl f')

-- |Open a capture stream from a movie file. The returned action may
-- be used to query for the next available frame. The sequence of
-- frames will return to its beginning when the end of the video is
-- encountered.
createFileCaptureLoop :: (HasDepth d, SingI c) =>
                         FilePath -> IO (IO (Image c d NoROI))
createFileCaptureLoop fname = do capture <- createFileCaptureF fname
                                 return (withForeignPtr capture $ 
                                         (>>= peekIpl) . queryFrameLoop)


-- |Open a capture stream from a connected camera. The parameter is
-- the index of the camera to be used, or 'Nothing' if it does not
-- matter what camera is used. The returned action may be used to
-- query for the next available frame.
createCameraCapture :: (HasDepth d, SingI c) => 
                       Maybe Int -> IO (IO (Image c d NoROI))
createCameraCapture cam = do cvInit
                             capture <- createCameraCaptureF cam'
                             return (withForeignPtr capture $ 
                                     (>>= peekIpl) . queryError)
    where cam' = fromMaybe (-1) cam

-- |4-character code for MPEG-4.
mpeg4CC :: FourCC
mpeg4CC = ('F','M','P','4')

-- |Create a video file writer. The parameters are the file name, the
-- 4-character code (of the codec used to compress the frames
-- (e.g. @(\'F\',\'M\',\'P\',\'4\')@ for MPEG-4), the framerate of the
-- created video stream, and the size of the video frames. The
-- returned action may be used to add frames to the video stream.
createVideoWriter :: (HasDepth d, UpdateROI r) =>
                     FilePath -> FourCC -> Double -> (Int,Int) -> 
                     IO (Image Trichromatic d r -> IO ())
createVideoWriter fname codec fps sz = 
    do writer <- createVideoWriterF fname codec fps sz
       let writeFrame img = withForeignPtr writer $ \writer' ->
                              withIplImage img $ \img' ->
                                cvWriteFrame writer' img'
       return writeFrame
