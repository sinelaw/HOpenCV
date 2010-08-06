-- |High-level Haskell bindings to OpenCV operations. Some of these
-- operations will be performed in-place under composition. For
-- example, @dilate 8 . erode 8@ will allocate one new image rather
-- than two.
module AI.CV.OpenCV.HighCV (erode, dilate, houghStandard, houghProbabilistic, 
                            LineType(..), RGB, drawLines, HIplImage, width, 
                            height, pixels, fromGrayPixels, fromColorPixels, 
                            fromFileGray, fromFileColor, toFile, findContours, 
                            fromPtr, isColor, isMono, fromPixels, sampleLine,
                            Connectivity(..), fromPixelsCopy, 
                            module AI.CV.OpenCV.HighColorConv)
    where
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HighColorConv
import AI.CV.OpenCV.HIplUtils
import Control.Monad.ST (runST, unsafeIOToST)
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce

-- |Erode an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
erode :: (HasChannels c, HasDepth d, Storable d) =>
         Int -> HIplImage a c d -> HIplImage FreshImage c d
erode n img = runST $
              unsafeIOToST . withHIplImage img $
              \src -> return . fst . withCompatibleImage img $
                      \dst -> cvErode src dst n'
    where n' = fromIntegral n

-- |Dilate an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
dilate :: (HasChannels c, HasDepth d, Storable d) =>
          Int -> HIplImage a c d -> HIplImage FreshImage c d
dilate n img = runST $ 
               unsafeIOToST . withHIplImage img $
               \src -> return . fst . withCompatibleImage img $
                       \dst -> cvDilate src dst n'
    where n' = fromIntegral n

-- |Unsafe in-place erosion. This is a destructive update of the given
-- image and is only used by the fusion rewrite rules when there is
-- no way to observe the input image.
unsafeErode :: (HasChannels c, HasDepth d, Storable d) =>
               Int -> HIplImage a c d -> HIplImage FreshImage c d
unsafeErode n img = runST $ 
                    unsafeIOToST $
                        withHIplImage img (\src -> cvErode src src n') >> 
                        return (unsafeCoerce img)
    where n' = fromIntegral n

-- |Unsafe in-place dilation. This is a destructive update of the
-- given image and is only used by the fusion rewrite rules when
-- there is no way to observe the input image.
unsafeDilate :: (HasChannels c, HasDepth d, Storable d) =>
                Int -> HIplImage a c d-> HIplImage FreshImage c d
unsafeDilate n img = runST $ 
                     unsafeIOToST $
                         withHIplImage img (\src -> cvDilate src src n') >> 
                         return (unsafeCoerce img)
    where n' = fromIntegral n

-- Perform destructive in-place updates when such a change is
-- safe. Safety is indicated by the phantom type tag annotating
-- HIplImage. If we have a function yielding an HIplImage FreshImage,
-- then we can clobber it. That is the *only* time these in-place
-- operations are known to be safe.

{-# RULES 
"erode-in-place"  forall n (f::a -> HIplImage FreshImage c d). erode n . f = unsafeErode n . f
"dilate-in-place" forall n (f::a -> HIplImage FreshImage c d). dilate n . f = unsafeDilate n . f
  #-}

-- |Extract all the pixel values from an image along a line, including
-- the end points. Takes two points, the line connectivity to use when
-- sampling, and an image; returns the list of pixel values.
sampleLine :: (HasChannels c, HasDepth d, Storable d) =>
              (Int, Int) -> (Int, Int) -> Connectivity -> HIplImage a c d -> [d]
sampleLine pt1 pt2 conn img = runST $ unsafeIOToST $ 
                              withHIplImage img $ 
                                \p -> cvSampleLine p pt1 pt2 conn

-- |Line detection in a binary image using a standard Hough transform.
houghStandard :: Double -> Double -> Int -> HIplImage a MonoChromatic Word8 -> 
                 [((Int, Int),(Int,Int))]
houghStandard rho theta threshold img = runST $ unsafeIOToST $
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

-- |Line detection in a binary image using a probabilistic Hough transform.
houghProbabilistic :: Double -> Double -> Int -> Double -> Double -> 
                      HIplImage a MonoChromatic Word8 -> [((Int, Int),(Int,Int))]
houghProbabilistic rho theta threshold minLength maxGap img = 
    runST $ unsafeIOToST $
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
-- and aliasing style. This function is fusible under composition.
drawLines :: (HasChannels c, HasDepth d, Storable d) =>
             RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
             HIplImage a c d -> HIplImage FreshImage c d
drawLines col thick lineType lines img = 
    fst $ withDuplicateImage img $ \ptr -> mapM_ (draw ptr) lines
    where draw ptr (pt1, pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

-- |Unsafe in-place line drawing.
unsafeDrawLines :: (HasChannels c, HasDepth d, Storable d) =>
                   RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
                   HIplImage a c d -> HIplImage FreshImage c d
unsafeDrawLines col thick lineType lines img = 
    runST $ unsafeIOToST $
    withHIplImage img $ \ptr -> mapM_ (draw ptr) lines >> return (unsafeCoerce img)
    where draw ptr (pt1,pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

{-# RULES
  "draw-lines-in-place" forall c t lt lns (f::a -> HIplImage FreshImage c d). 
  drawLines c t lt lns . f = unsafeDrawLines c t lt lns . f
  #-}

-- |Find the 'CvContour's in an image.
findContours :: HIplImage a MonoChromatic Word8 -> [CvContour]
findContours img = snd $ withDuplicateImage img $
                     \src -> cvFindContours src CV_RETR_CCOMP CV_CHAIN_APPROX_SIMPLE
