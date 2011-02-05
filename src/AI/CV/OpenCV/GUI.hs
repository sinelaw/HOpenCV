module AI.CV.OpenCV.GUI (namedWindow, WindowFlag(..), MouseCallback, 
                         waitKey, cvInit) where
import AI.CV.OpenCV.Core.HIplImage
import AI.CV.OpenCV.Core.HighGui
import AI.CV.OpenCV.Core.CxCore (fromArr)
import Control.Monad ((>=>))
import Foreign.C.String (newCString)

--type KeyboardCallback = Int -> IO ()

-- |Create a new window with the given title. The return value is an
-- action for destroying the window.
namedWindow :: (HasChannels c, HasDepth d) => 
               String -> [WindowFlag] -> 
               Maybe MouseCallback -> 
               --Maybe KeyboardCallback ->
               IO (HIplImage c d -> IO (), IO ())
namedWindow name flags _cb =
  do cstr <- newCString name
     let showImg img = withHIplImage img $ \imgPtr ->
                         cvShowImage cstr (fromArr imgPtr)
     cvNamedWindow cstr (windowFlagsToEnum flags)
     return (showImg, cvDestroyWindow cstr)

-- | @waitKey delay@ waits for a key indifinitely if @delay <= 0@, or
-- for @delay@ milliseconds. The returned value is the code of the
-- pressed key or 'Nothing'.
waitKey :: Int -> IO (Maybe Int)
waitKey = cvWaitKey . fromIntegral >=> return . checkKey
  where checkKey (-1) = Nothing
        checkKey x = Just (fromIntegral x)
