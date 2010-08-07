{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables #-}
-- |Support for features from the OpenCV Image Filtering library.
module AI.CV.OpenCV.CV 
    ( InterpolationMethod(..),
      cvCanny, cvResize, cvDilate, cvErode, cvPyrDown, cvHoughLines2, 
      CvHaarClassifierCascade, HaarDetectFlag,
      cvHaarFlagNone, cvHaarDoCannyPruning, 
      cvHaarScaleImage, cvHaarFindBiggestObject, cvHaarDoRoughSearch,
      combineHaarFlags, cvHaarDetectObjects,
      cvCvtColor, cvFindContours, ContourMethod(..), ContourMode(..),
      cvSampleLine, Connectivity(..)
    ) where

import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (poke, peek, peekByteOff, Storable(..))
import Foreign.Ptr
import Data.Bits
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.ColorConversion

#include <opencv/cv.h>

foreign import ccall unsafe "opencv/cv.h cvCanny"
  c_cvCanny :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> CInt -> IO ()

-- Canny 
cvCanny :: (IplArrayType i1, IplArrayType i2) =>
           Ptr i1 -> Ptr i2 -> Double -> Double -> Int -> IO ()
cvCanny src dst threshold1 threshold2 apertureSize = 
  c_cvCanny (fromArr src) (fromArr dst) (realToFrac threshold1) 
            (realToFrac threshold2) (fromIntegral apertureSize)


data InterpolationMethod = CV_INTER_NN 
                         | CV_INTER_LINEAR 
                         | CV_INTER_CUBIC
                         | CV_INTER_AREA
                           deriving (Enum,Eq)

foreign import ccall unsafe "opencv/cv.h cvResize"
  c_cvResize :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

cvResize :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> InterpolationMethod -> IO ()
cvResize src dst interp = c_cvResize (fromArr src) (fromArr dst) (fromIntegral . fromEnum $ interp)

foreign import ccall unsafe "opencv/cv.h cvDilate"
  c_dilate :: Ptr CvArr -> Ptr CvArr -> Ptr () -> CInt -> IO ()

-- |Dilate the first image using a 3x3 rectangular structuring element
-- and store the result in the second image. The third parameter is
-- the number of dilation iterations to perform.
cvDilate :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2  -> CInt -> IO ()
cvDilate src dst iter = c_dilate (fromArr src) (fromArr dst) nullPtr iter

foreign import ccall unsafe "opencv/cv.h cvErode"
  c_erode :: Ptr CvArr -> Ptr CvArr -> Ptr () -> CInt -> IO ()

-- |Erode the first image using a 3x3 rectangular structuring element
-- and store the result in the second image. The third parameter is
-- the number of erosion iterations to perform.
cvErode :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> CInt -> IO ()
cvErode src dst iter = c_erode (fromArr src) (fromArr dst) nullPtr iter

foreign import ccall unsafe "opencv/cv.h cvHoughLines2"
        c_cvHoughLines2 :: Ptr CvArr -> Ptr CvMemStorage -> CInt -> CDouble -> CDouble -> CInt -> CDouble -> CDouble -> IO (Ptr (CvSeq a))

cvHoughLines2 :: IplArrayType i => Ptr i -> Ptr CvMemStorage -> CInt -> Double -> Double -> Int -> Double -> Double -> IO (Ptr (CvSeq a))
cvHoughLines2 img storage method rho theta threshold param1 param2 = 
    c_cvHoughLines2 (fromArr img) storage method (realToFrac rho) 
                    (realToFrac theta) (fromIntegral threshold) 
                    (realToFrac param1) (realToFrac param2)

foreign import ccall unsafe "opencv/cv.h cvCvtColor"
  c_cvCvtColor :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

foreign import ccall unsafe "opencv/cv.h cvSampleLine"
  c_cvSampleLine :: Ptr CvArr -> CInt -> CInt -> CInt -> CInt -> Ptr a -> 
                    CInt -> IO CInt

-- |Line connectivity used for sampling.
data Connectivity = Four | Eight

-- |Read all of the image points lying on the line between pt1 and
-- pt2, including the end points. Takes an image, two points, and the
-- line connectivity; returns all the pixel values along that line.
cvSampleLine :: forall a b. (IplArrayType a, Storable b) => 
                Ptr a -> (Int,Int) -> (Int,Int) -> Connectivity -> IO [b]
cvSampleLine img (x1,y1) (x2,y2) c = 
    do nc <- getNumChannels (castPtr img)
       allocaBytes (sz'*nc) $ 
         \buffer -> do n <- c_cvSampleLine (fromArr img) (fi x1) (fi y1) 
                                           (fi x2) (fi y2) buffer c'
                       peekArray ((fromIntegral n)*nc) buffer
  where fi = fromIntegral
        (sz,c') = case c of
                    Four -> (abs (x2 - x1) + abs (y2 - y1) + 1, 4)
                    Eight -> (max (abs (x2 - x1) + 1) (abs (y2 - y1) + 1), 8)
        sz' = sizeOf (undefined::b) * sz

-- |Convert the color of the first 'IplImage', storing the result in
-- the second. The second image must have the same dimensions as the
-- first and the same depth, but it's number of color channels may be
-- different and must be compatible with the given 'ColorConversion'
-- code.
cvCvtColor :: (IplArrayType a, IplArrayType b) => 
              Ptr a -> Ptr b -> ColorConversion -> IO ()
cvCvtColor src dst code = c_cvCvtColor (fromArr src) (fromArr dst) (colorConv code)


foreign import ccall unsafe "HOpenCV_wrap.h c_cvFindContours"
  c_cvFindContours :: Ptr CvArr -> Ptr CvMemStorage -> Ptr (Ptr (CvSeq a)) -> Int -> Int -> Int -> Int -> Int -> IO Int

-- |Contour extraction mode.
data ContourMode = CV_RETR_EXTERNAL -- ^retrieves only the extreme
                                    -- outer contours

                 | CV_RETR_LIST     -- ^retrieves all of the contours
                                    -- and puts them in the list

                 | CV_RETR_CCOMP    -- ^retrieves all of the contours
                                    -- and organizes them into a
                                    -- two-level hierarchy: on the top
                                    -- level are the external
                                    -- boundaries of the components,
                                    -- on the second level are the
                                    -- boundaries of the holes

                 | CV_RETR_TREE     -- ^retrieves all of the contours
                                    -- and reconstructs the full
                                    -- hierarchy of nested contours
                   deriving (Enum, Eq)

data ContourMethod = CV_CHAIN_APPROX_NONE      
                   -- ^translates all of the points from the chain
                   -- code into points

                   | CV_CHAIN_APPROX_SIMPLE    
                   -- ^compresses horizontal, vertical, and diagonal
                   -- segments and leaves only their end points

                   | CV_CHAIN_APPROX_TC89_L1   
                   -- ^applies one of the flavors of the Teh-Chin
                   -- chain approximation algorithm

                   | CV_CHAIN_APPROX_TC89_KCOS
                   -- ^applies one of the flavors of the Teh-Chin
                   -- chain approximation algorithm

                   | CV_LINK_RUNS
                   -- ^uses a completely different contour retrieval
                   -- algorithm by linking horizontal segments of
                   -- 1's. Only the CV_RETR_LIST retrieval mode can be
                   -- used with this method.  

                   -- | CV_CHAIN_CODE -- changes returned sequence type
                     deriving Enum

-- |The function retrieves 'CvContour's from the binary image using the
-- algorithm Suzuki85. The contours are a useful tool for shape
-- analysis and object detection and recognition.
cvFindContours :: IplArrayType a => Ptr a -> ContourMode -> ContourMethod -> IO [CvContour]
cvFindContours img mode method = 
    do storage <- cvCreateMemStorage 0
       let header = case method of
                      --CV_CHAIN_CODE -> (#size CvChain)
                      _ -> (#size CvContour)
           mode' = fromEnum mode
           method' = case method of 
                       CV_LINK_RUNS -> if mode == CV_RETR_LIST
                                       then fromEnum method
                                       else error $ "CV_LINK_RUNS can only be "++
                                                    "used with CV_RETR_LIST"
                       _ -> fromEnum method
       cs <- alloca $ \cseq ->
               do _n <- alloca $ \cseq' ->
                          poke (cseq'::Ptr (Ptr CInt)) cseq >>
                          c_cvFindContours (fromArr img) storage (castPtr cseq')
                                           header mode' method' 0 0
                  putStrLn $ "Found "++show _n++" contours"
                  followContourList (castPtr cseq)
       cvReleaseMemStorage storage
       return cs

-- FIXME: This is wrong. We're actually getting an array of arrays of
-- Points. Check the cvDrawContours function to see how to interpret
-- the result of c_cvFindContours.
followContourList :: Ptr (CvSeq CvContour) -> IO [CvContour]
followContourList = go []
    where go acc p = if p == nullPtr
                     then return $ reverse acc
                     else do putStrLn "Getting element 1"
                             n <- seqNumElems p
                             putStrLn $ "Initial seq has "++show n++" elems"
                             x <- peek =<< cvGetSeqElem p 1
                             putStrLn $ "Found " ++ show x
                             p' <- (#peek CvSeq, h_next) p
                             go (x:acc) p'

foreign import ccall unsafe "opencv/cv.h cvPyrDown"
  c_cvPyrDown :: Ptr CvArr -> Ptr CvArr -> CInt -> IO ()

-- for now only one filter type is supported so no need for the CInt (filter type)
constCvGaussian5x5 :: CInt
constCvGaussian5x5 = 7
cvPyrDown :: (IplArrayType i1, IplArrayType i2) => Ptr i1 -> Ptr i2 -> IO ()
cvPyrDown src dst = c_cvPyrDown (fromArr src) (fromArr dst) constCvGaussian5x5

------------------------------------------------------------------------------

data CvHaarClassifierCascade

-- thanks to http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
newtype HaarDetectFlag = HaarDetectFlag { unHaarDetectFlag :: CInt }
    deriving (Eq, Show)
             
#{enum HaarDetectFlag, HaarDetectFlag
 , cvHaarFlagNone = 0
 , cvHaarDoCannyPruning    = CV_HAAR_DO_CANNY_PRUNING 
 , cvHaarScaleImage        = CV_HAAR_SCALE_IMAGE 
 , cvHaarFindBiggestObject = CV_HAAR_FIND_BIGGEST_OBJECT 
 , cvHaarDoRoughSearch     = CV_HAAR_DO_ROUGH_SEARCH
 }

combineHaarFlags :: [HaarDetectFlag] -> HaarDetectFlag
combineHaarFlags = HaarDetectFlag . foldr ((.|.) . unHaarDetectFlag) 0
  
foreign import ccall unsafe "HOpenCV_wrap.h c_cvHaarDetectObjects"
  c_cvHaarDetectObjects :: Ptr CvArr   -- ^ image
                        -> Ptr CvHaarClassifierCascade -- ^ cascade
                        -> Ptr CvMemStorage            -- ^ storage
                        -> CDouble                     -- ^ scale_factor
                        -> CInt                        -- ^ min_neighbors
                        -> CInt                        -- ^ flags
                        -> CInt -> CInt                -- ^ min_size
                        -> IO (Ptr (CvSeq CvRect))

cvHaarDetectObjects :: (IplArrayType i) => 
                           Ptr i                       -- ^ image
                        -> Ptr CvHaarClassifierCascade -- ^ cascade
                        -> Ptr CvMemStorage            -- ^ storage
                        -> CDouble                     -- ^ scale_factor
                        -> CInt                        -- ^ min_neighbors
                        -> HaarDetectFlag              -- ^ flags
                        -> CvSize                      -- ^ min_size
                        -> IO (Ptr (CvSeq CvRect))
cvHaarDetectObjects image cascade storage scaleFactor minNeighbors flags minSize = 
  c_cvHaarDetectObjects (fromArr image) cascade storage scaleFactor minNeighbors (unHaarDetectFlag flags) (sizeWidth minSize) (sizeHeight minSize)
  