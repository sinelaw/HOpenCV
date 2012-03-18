module AI.CV.OpenCV.Unfinalized.CxCore
where

import AI.CV.OpenCV.CxCore

type CvMemStorageU = Ptr Priv_CvMemStorage

type CvSeqU a      = Ptr (Priv_CvSeq a)

--------------------------------------------------------------------------------
-- IplImageU

newtype IplImageU = IplImageU (Ptr Priv_IplImage)

foreign import ccall unsafe "HOpenCV_wrap.h release_image"
  cvReleaseImage :: Ptr Priv_IplImage -> IO ()

createImageU :: CvSize -> Int -> Depth -> IO IplImageU
createImageU size numChans depth
  = do im <- errorName "Failed to create image" . checkPtr 
             $ c_cvCreateImage (sizeWidth size) (sizeHeight size)
                               (unDepth depth)
                               (fromIntegral numChans)
       return $ IplImageU im

releaseImage :: IplImageU -> IO ()
releaseImage (IplImageU p) = cvReleaseImage p 

cloneImage :: IplImageU-> IO IplImageU
cloneImage (IplImageU p)
  = do p' <- errorName "Failed to clone image" . checkPtr 
             . c_cvCloneImage $ p
       return $ IplImageU p' 

