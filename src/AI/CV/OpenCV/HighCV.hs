-- |High-level Haskell bindings to OpenCV operations. Some of these
-- operations will be performed in-place under composition. For
-- example, @dilate 8 . erode 8@ will allocate one new image rather
-- than two.
module AI.CV.OpenCV.HighCV (erode, dilate, houghStandard, houghProbabilistic, 
                            LineType(..), RGB, drawLines, HIplImage, width, 
                            height, pixels, withPixels, fromGrayPixels, 
                            fromColorPixels, fromFileGray, fromFileColor, 
                            toFile, fromPtr, isColor, isMono, HasDepth, 
                            withImagePixels, sampleLine, Connectivity(..), 
                            fromPixels, cannyEdges, createFileCapture, 
                            createCameraCapture, resize, FourCC, getROI,
                            InterpolationMethod(..), MonoChromatic, 
                            TriChromatic, createVideoWriter, HasChannels,
                            module AI.CV.OpenCV.ColorConversion, 
                            createFileCaptureLoop)
    where
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.CV
import AI.CV.OpenCV.Core.HighGui (createFileCaptureF, cvQueryFrame, 
                                  setCapturePos, CapturePos(PosFrames), 
                                  CvCapture, createCameraCaptureF, 
                                  createVideoWriterF, cvWriteFrame, FourCC)
import AI.CV.OpenCV.Core.HIplUtils
import AI.CV.OpenCV.ColorConversion
--import AI.CV.OpenCV.Contours
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

-- |Erode an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
erode :: (HasChannels c, HasDepth d) =>
         Int -> HIplImage c d -> HIplImage c d
erode n img = unsafePerformIO . withHIplImage img $
              \src -> return . fst . withCompatibleImage img $
                      \dst -> cvErode src dst n'
    where n' = fromIntegral n

-- |Dilate an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
dilate :: (HasChannels c, HasDepth d) =>
          Int -> HIplImage c d -> HIplImage c d
dilate n img = unsafePerformIO . withHIplImage img $
               \src -> return . fst . withCompatibleImage img $
                       \dst -> cvDilate src dst n'
    where n' = fromIntegral n

-- |Unsafe in-place erosion. This is a destructive update of the given
-- image and is only used by the rewrite rules when there is no way to
-- observe the input image.
unsafeErode :: (HasChannels c, HasDepth d) => 
               Int -> HIplImage c d -> HIplImage c d
unsafeErode n img = unsafePerformIO $
                    withHIplImage img (\src -> cvErode src src n') >> 
                    return (unsafeCoerce img)
    where n' = fromIntegral n

-- |Unsafe in-place dilation. This is a destructive update of the
-- given image and is only used by the rewrite rules when there is no
-- way to observe the input image.
unsafeDilate :: (HasChannels c, HasDepth d) =>
                Int -> HIplImage c d-> HIplImage c d
unsafeDilate n img = unsafePerformIO $
                     withHIplImage img (\src -> cvDilate src src n') >> 
                     return (unsafeCoerce img)
    where n' = fromIntegral n

-- Perform destructive in-place updates when such a change is
-- safe. Safety is indicated by the phantom type tag annotating
-- HIplImage. If we have a function yielding an HIplImage FreshImage,
-- then we can clobber it. That is the *only* time these in-place
-- operations are known to be safe.

{-# RULES 
"erode/in-place"  forall n. erode n = pipeline (unsafeErode n)
"dilate/in-place" forall n. dilate n = pipeline (unsafeDilate n)
  #-}

-- |Extract all the pixel values from an image along a line, including
-- the end points. Parameters are the two endpoints, the line
-- connectivity to use when sampling, and an image; returns the list
-- of pixel values.
sampleLine :: (HasChannels c, HasDepth d) =>
              (Int, Int) -> (Int, Int) -> Connectivity -> HIplImage c d -> [d]
sampleLine pt1 pt2 conn img = unsafePerformIO . withHIplImage img $ 
                                \p -> cvSampleLine p pt1 pt2 conn

-- |Line detection in a binary image using a standard Hough
-- transform. Parameters are @rho@, the distance resolution in
-- pixels; @theta@, the angle resolution in radians; @threshold@, the
-- line classification accumulator threshold; and the input image.
houghStandard :: Double -> Double -> Int -> HIplImage MonoChromatic Word8 -> 
                 [((Int, Int),(Int,Int))]
houghStandard rho theta threshold img = unsafePerformIO $
    do storage <- cvCreateMemStorage (min 0 (fromIntegral threshold))
       cvSeq <- withHIplImage img $ 
                \p -> cvHoughLines2 p storage 0 rho theta threshold 0 0
       hlines <- mapM (\p -> do f1 <- peek p
                                f2 <- peek (plusPtr p (sizeOf (undefined::Float)))
                                return (f1,f2))
                      =<< seqToPList cvSeq
       cvReleaseMemStorage storage
       return $ map lineToSeg hlines
    where lineToSeg :: (Float,Float) -> ((Int,Int),(Int,Int))
          lineToSeg (rho, theta) = let a = cos theta
                                       b = sin theta
                                       x0 = a * rho
                                       y0 = b * rho
                                       x1 = clampX $ x0 + 10000*(-b)
                                       y1 = clampY $ y0 + 10000*a
                                       x2 = clampX $ x0 - 10000*(-b)
                                       y2 = clampY $ y0 - 10000*a
                                   in ((x1,y1),(x2,y2))
          clampX x = max 0 (min (truncate x) (width img - 1))
          clampY y = max 0 (min (truncate y) (height img - 1))

-- |Line detection in a binary image using a probabilistic Hough
-- transform. Parameters are @rho@, the distance resolution in pixels;
-- @theta@, the angle resolution in radians; @threshold@, the line
-- classification accumulator threshold; and the input image.
houghProbabilistic :: Double -> Double -> Int -> Double -> Double -> 
                      HIplImage MonoChromatic Word8 -> [((Int, Int),(Int,Int))]
houghProbabilistic rho theta threshold minLength maxGap img = 
    unsafePerformIO $
    do storage <- cvCreateMemStorage (min 0 (fromIntegral threshold))
       let cvSeq = snd $ withDuplicateImage img $
                     \p -> cvHoughLines2 p storage 1 rho theta threshold
                                         minLength maxGap
       hlines <- mapM (\p1 -> do x1 <- peek p1
                                 let p2 = plusPtr p1 step
                                     p3 = plusPtr p2 step
                                     p4 = plusPtr p3 step
                                 y1 <- peek p2
                                 x2 <- peek p3
                                 y2 <- peek p4
                                 return ((x1,y1),(x2,y2)))
                      =<< seqToPList cvSeq
       cvReleaseMemStorage storage
       return hlines
    where step = sizeOf (undefined::Int)

-- |Type of line to draw.
data LineType = EightConn -- ^8-connected line
              | FourConn  -- ^4-connected line
              | AALine    -- ^antialiased line

-- |An RGB triple. 
type RGB = (Double, Double, Double)

-- |Convert a LineType into an integer.
lineTypeEnum :: LineType -> Int
lineTypeEnum EightConn = 8
lineTypeEnum FourConn  = 4
lineTypeEnum AALine    = 16

-- |Draw each line, defined by its endpoints, on a duplicate of the
-- given 'HIplImage' using the specified RGB color, line thickness,
-- and aliasing style.
drawLines :: (HasChannels c, HasDepth d) =>
             RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
             HIplImage c d -> HIplImage c d
drawLines col thick lineType lines img = 
    fst $ withDuplicateImage img $ \ptr -> mapM_ (draw ptr) lines
    where draw ptr (pt1, pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

-- |Unsafe in-place line drawing.
unsafeDrawLines :: (HasChannels c, HasDepth d) =>
                   RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
                   HIplImage c d -> HIplImage c d
unsafeDrawLines col thick lineType lines img = 
    unsafePerformIO . withHIplImage img $ \ptr -> 
        mapM_ (draw ptr) lines >> return (unsafeCoerce img)
    where draw ptr (pt1,pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

{-# RULES "drawLines/in-place" forall c t lt lns. 
  drawLines c t lt lns = pipeline (unsafeDrawLines c t lt lns)
  #-}

-- |Find edges using the Canny algorithm. The smallest value between
-- threshold1 and threshold2 (the first two parameters, respectively)
-- is used for edge linking, the largest value is used to find the
-- initial segments of strong edges. The third parameter is the
-- aperture parameter for the Sobel operator.
cannyEdges :: HasDepth d =>
              Double -> Double -> Int -> HIplImage MonoChromatic d -> 
              HIplImage MonoChromatic d
cannyEdges threshold1 threshold2 aperture img = 
    fst . withCompatibleImage img $ \dst -> 
        withHIplImage img $ \src -> 
            cvCanny src dst threshold1 threshold2 aperture

unsafeCanny :: HasDepth d =>
               Double -> Double -> Int -> HIplImage MonoChromatic d -> 
               HIplImage MonoChromatic d
unsafeCanny threshold1 threshold2 aperture img = 
    unsafePerformIO . withHIplImage img $ \src -> 
        cvCanny src src threshold1 threshold2 aperture >> return img

{-# RULES "canny/in-place" forall t1 t2 a.
  cannyEdges t1 t2 a = pipeline (unsafeCanny t1 t2 a)
  #-}

{-
-- |Find the 'CvContour's in an image.
findContours :: HIplImage a MonoChromatic Word8 -> [CvContour]
findContours img = snd $ withDuplicateImage img $
                     \src -> cvFindContours src CV_RETR_CCOMP CV_CHAIN_APPROX_SIMPLE
-}

-- |Raise an error if 'cvQueryFrame' returns 'Nothing'; otherwise
-- returns a 'Ptr' 'IplImage'.
queryError :: Ptr CvCapture -> IO (Ptr IplImage)
queryError = (maybe (error "Unable to capture frame") id `fmap`) . cvQueryFrame

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
createFileCapture :: (HasChannels c, HasDepth d) => 
                     FilePath -> IO (IO (Maybe (HIplImage c d)))
createFileCapture fname = do capture <- createFileCaptureF fname
                             return (withForeignPtr capture $ \cap ->
                                       do f <- cvQueryFrame cap
                                          case f of
                                            Nothing -> return Nothing
                                            Just f' -> Just `fmap` fromPtr f')

-- |Open a capture stream from a movie file. The returned action may
-- be used to query for the next available frame. The sequence of
-- frames will return to its beginning when the end of the video is
-- encountered.
createFileCaptureLoop :: (HasChannels c, HasDepth d) =>
                         FilePath -> IO (IO (HIplImage c d))
createFileCaptureLoop fname = do capture <- createFileCaptureF fname
                                 return (withForeignPtr capture $ 
                                         (>>= fromPtr) . queryFrameLoop)


-- |Open a capture stream from a connected camera. The parameter is
-- the index of the camera to be used, or 'Nothing' if it does not
-- matter what camera is used. The returned action may be used to
-- query for the next available frame.
createCameraCapture :: (HasChannels c, HasDepth d) =>
                       Maybe Int -> IO (IO (HIplImage c d))
createCameraCapture cam = do capture <- createCameraCaptureF cam'
                             return (withForeignPtr capture $ 
                                     (>>= fromPtr) . queryError)
    where cam' = maybe (-1) id cam

-- |Create a video file writer. The parameters are the file name, the
-- 4-character code (of the codec used to compress the frames
-- (e.g. @(\'F\',\'M\',\'P\',\'4\')@ for MPEG-4), the framerate of the
-- created video stream, and the size of the video frames. The
-- returned action may be used to add frames to the video stream.
createVideoWriter :: (HasChannels c, HasDepth d) =>
                     FilePath -> FourCC -> Double -> (Int,Int) -> 
                     IO (HIplImage c d -> IO ())
createVideoWriter fname codec fps sz = 
    do writer <- createVideoWriterF fname codec fps sz
       let writeFrame img = withForeignPtr writer $ \writer' ->
                              withHIplImage img $ \img' ->
                                cvWriteFrame writer' img'
       return writeFrame

-- |Resize the supplied 'HIplImage' to the given width and height using
-- the supplied 'InterpolationMethod'.
resize :: (HasChannels c, HasDepth d) => 
          InterpolationMethod -> Int -> Int -> HIplImage c d -> HIplImage c d
resize method w h img = 
    unsafePerformIO $
    do img' <- mkHIplImage w h
       _ <- withHIplImage img $ \src ->
              withHIplImage img' $ \dst ->
                cvResize src dst method
       return img'
