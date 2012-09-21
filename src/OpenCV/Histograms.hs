{-# LANGUAGE ForeignFunctionInterface #-}
module OpenCV.Histograms (equalizeHist) where
import Foreign.Ptr (Ptr)
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp

foreign import ccall "opencv2/imgproc/imgproc_c.h cvEqualizeHist"
  c_cvEqualizeHist :: Ptr CvArr -> Ptr CvArr -> IO ()

equalizeHist :: GrayImage -> GrayImage
equalizeHist = cv $ \src -> c_cvEqualizeHist src src
