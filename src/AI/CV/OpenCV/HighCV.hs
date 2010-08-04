-- |High-level Haskell bindings to OpenCV operations. Some of these
-- operations will be performed in-place under composition. For
-- example, @dilate 8 . erode 8@ will allocate one new image rather
-- than two.
module AI.CV.OpenCV.HighCV (erode, dilate, houghStandard, houghProbabilistic, 
                            LineType(..), RGB, drawLines, convertColor) 
    where
import AI.CV.OpenCV.ColorConversion
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HIplImage
import Control.Monad.ST (runST, unsafeIOToST)
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce

-- |Erode an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
erode :: Int -> HIplImage a -> HIplImage FreshImage
erode n img = runST $
              unsafeIOToST . withHIplImage img $
              \src -> return . withCompatibleImage img $
                      \dst -> cvErode src dst n'
    where n' = fromIntegral n

-- |Dilate an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
dilate :: Int -> HIplImage a -> HIplImage FreshImage
dilate n img = runST $ 
               unsafeIOToST . withHIplImage img $
               \src -> return . withCompatibleImage img $
                       \dst -> cvDilate src dst n'
    where n' = fromIntegral n

-- |Unsafe in-place erosion. This is a destructive update of the given
-- image and is only used by the fusion rewrite rules when there is
-- no way to observe the input image.
unsafeErode :: Int -> HIplImage a -> HIplImage FreshImage
unsafeErode n img = runST $ 
                    unsafeIOToST $
                        withHIplImage img (\src -> cvErode src src n') >> 
                        return (unsafeCoerce img)
    where n' = fromIntegral n

-- |Unsafe in-place dilation. This is a destructive update of the
-- given image and is only used by the fusion rewrite rules when
-- there is no way to observe the input image.
unsafeDilate :: Int -> HIplImage a -> HIplImage FreshImage
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
"erode-in-place"  forall n (f::a->HIplImage FreshImage). erode n . f = unsafeErode n . f
"dilate-in-place" forall n (f::a->HIplImage FreshImage). dilate n . f = unsafeDilate n . f
  #-}

-- |Line detection in a binary image using a standard Hough transform.
houghStandard :: Double -> Double -> Int -> HIplImage a -> [((Int, Int),(Int,Int))]
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
                      HIplImage a -> [((Int, Int),(Int,Int))]
houghProbabilistic rho theta threshold minLength maxGap img = 
    runST $ unsafeIOToST $
    do storage <- cvCreateMemStorage (min 0 (fromIntegral threshold))
       cvSeq <- withHIplImage img $
                \p -> cvHoughLines2 p storage 1 rho theta threshold minLength 
                                    maxGap
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
drawLines :: RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
             HIplImage a -> HIplImage FreshImage
drawLines col thick lineType lines img = 
    withDuplicateImage img $ \ptr -> mapM_ (draw ptr) lines
    where draw ptr (pt1, pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

-- |Unsafe in-place line drawing.
unsafeDrawLines :: RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
                   HIplImage a -> HIplImage FreshImage
unsafeDrawLines col thick lineType lines img = 
    runST $ unsafeIOToST $
    withHIplImage img $ \ptr -> mapM_ (draw ptr) lines >> return (unsafeCoerce img)
    where draw ptr (pt1,pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType

{-# RULES
  "draw-lines-in-place" forall c t lt lns (f::a->HIplImage FreshImage). 
  drawLines c t lt lns . f = unsafeDrawLines c t lt lns . f
  #-}

-- |Convert the color model of an image.
convertColor :: ColorConversion -> HIplImage a -> HIplImage FreshImage
convertColor cc img = runST $ unsafeIOToST $ 
                       withHIplImage img $
                       \src -> do dst <- mkHIplImage w h nc
                                  withHIplImage dst $
                                    \dst' -> cvCvtColor src dst' cc
                                  return dst
    where w = width img
          h = height img
          destChannels = [(cv_RGB2BGR, 3), (cv_BGR2GRAY, 1), (cv_GRAY2BGR, 3)]
          nc = case lookup cc destChannels of
                 Just n -> n
                 Nothing -> error $ "Unfamiliar color conversion. "++
                                    "Contact maintainer."