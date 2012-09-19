{-# LANGUAGE ForeignFunctionInterface #-}
-- |Incomplete support for cvFindContours.
module AI.CV.OpenCV.Contours (ContourMode(..), ContourMethod(..), 
                              cvFindContours, followContourList) where
import AI.CV.OpenCV.Core.CxCore
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)

#include <opencv2/core/types_c.h>

foreign import ccall "HOpenCV_wrap.h c_cvFindContours"
  c_cvFindContours :: Ptr CvArr -> Ptr CvMemStorage -> Ptr (Ptr (CvSeq a)) -> 
                      Int -> Int -> Int -> Int -> Int -> IO Int

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

