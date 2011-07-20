{-# LANGUAGE ForeignFunctionInterface #-}
module AI.CV.OpenCV.Histograms (equalizeHist) where
import Foreign.Ptr (Ptr)
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp

foreign import ccall "opencv2/imgproc/imgproc_c.h cvEqualizeHist"
  c_cvEqualizeHist :: Ptr CvArr -> Ptr CvArr -> IO ()

equalizeHist :: GrayImage -> GrayImage
equalizeHist = cv $ \src -> c_cvEqualizeHist src src
