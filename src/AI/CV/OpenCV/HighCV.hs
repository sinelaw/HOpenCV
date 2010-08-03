-- High-level pure Haskell bindings to OpenCV operations.
module AI.CV.OpenCV.HCV where
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.CV
import AI.CV.OpenCV.HIplImage
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

-- NOTE: These operations allocate a fresh HIplImage on each invocation. The invocation is managed manually so that we obtain a pointer

-- |Erode an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
erode :: HIplImage -> Int -> HIplImage
erode img n = unsafePerformIO $
              withHIplImage img (\src -> return . withCompatibleImage img $
                                         \dst -> cvErode src dst n')
{-
erode img n = unsafePerformIO $ 
              do let destImg = compatibleImage img
                 withHIplImage img (\src -> withHIplImage destImg $
                                            \dst -> cvErode src dst n')
                 return destImg
-}
    where n' = fromIntegral n

-- |Dilate an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
dilate :: HIplImage -> Int -> HIplImage
dilate img n = unsafePerformIO $
               withHIplImage img (\src -> return . withCompatibleImage img $
                                          \dst -> cvDilate src dst n')
{-
dilate img n = unsafePerformIO $ 
               do destImg <- compatibleImage img
                  withHIplImage img (\src -> withHIplImage destImg $
                                             \dst -> cvDilate src dst n')
                  return destImg
-}
    where n' = fromIntegral n

-- |Line detection in a binary image using a standard Hough transform.
houghStandard :: Double -> Double -> Int -> HIplImage -> [(Float, Float)]
houghStandard rho theta threshold img = unsafePerformIO $
    do storage <- cvCreateMemStorage 0
       cvSeq <- withHIplImage img $ 
                \p -> cvHoughLines2 p storage 0 rho theta threshold 0 0
       hlines <- mapM (\p -> do f1 <- peek p
                                f2 <- peek (plusPtr p (sizeOf (undefined::Float)))
                                return (f1,f2))
                      =<< seqToPList cvSeq
       cvReleaseMemStorage storage
       return hlines

-- |Line detection in a binary image using a probabilistic Hough transform.
houghProbabilistic :: Double -> Double -> Int -> Double -> Double -> 
                      HIplImage -> [((Int, Int),(Int,Int))]
houghProbabilistic rho theta threshold minLength maxGap img = 
    unsafePerformIO $ do storage <- cvCreateMemStorage 0
                         cvSeq <- withHIplImage img $
                                  \p -> cvHoughLines2 p storage 1 rho theta 
                                                      threshold minLength 
                                                      maxGap
                         hlines <- mapM (\p1 -> do x1 <- peek p1
                                                   let step = sizeOf (undefined::Int)
                                                       p2 = plusPtr p1 step
                                                       p3 = plusPtr p2 step
                                                       p4 = plusPtr p3 step
                                                   y1 <- peek p2
                                                   x2 <- peek p3
                                                   y2 <- peek p4
                                                   return ((x1,y1),(x2,y2)))
                                         =<< seqToPList cvSeq
                         cvReleaseMemStorage storage
                         return hlines

-- |Type of line to draw.
data LineType = EightConn -- ^8-connected line
              | FourConn  -- ^4-connected line
              | AALine    -- ^antialiased line

-- |Convert a LineType into an integer.
lineTypeEnum :: LineType -> Int
lineTypeEnum EightConn = 8
lineTypeEnum FourConn  = 4
lineTypeEnum AALine    = 16

-- |Draw each line, defined by its endpoits, on top of a duplicate of
-- the given 'HIplImage'.
drawLines :: HIplImage -> [((Int,Int),(Int,Int))] -> HIplImage
drawLines img lines = withDuplicateImage img $
                      \ptr -> mapM_ (drawLine ptr (1,0,0) 2 8) lines
    where drawLine ptr col thick lineType (pt1, pt2) = 
              cvLine ptr pt1 pt2 col thick lineType