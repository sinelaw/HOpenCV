module AI.CV.OpenCV.GUI (namedWindow, WindowFlag(..), MouseCallback, 
                         waitKey, cvInit) where
import Control.Concurrent (forkIO, killThread, Chan, readChan)
import Control.Monad (forever, (>=>))
import AI.CV.OpenCV.Core.HIplImage
import AI.CV.OpenCV.Core.HighGui
import AI.CV.OpenCV.Core.CxCore (fromArr)
import Foreign.C.String (withCString)

-- |Create a new window with the given title. The return value is an
-- action for destroying the window.
namedWindow :: (HasChannels c, HasDepth d) => 
               String -> [WindowFlag] -> Maybe MouseCallback -> 
               Chan (HIplImage c d) -> IO (IO ())
namedWindow name flags _cb c = 
  withCString name $ \s ->
    do cvNamedWindow s (windowFlagsToEnum flags)
       t <- forkIO $ forever (readChan c >>= 
                              flip withHIplImage (cvShowImage s . fromArr) >>
                              waitKey 1 >> return ())
       return (killThread t >> cvDestroyWindow s)

-- | @waitKey delay@ waits for a key indifinitely if @delay <= 0@, or
-- for @delay@ milliseconds. The returned value is the code of the
-- pressed key or 'Nothing'.
waitKey :: Int -> IO (Maybe Int)
waitKey = cvWaitKey . fromIntegral >=> return . checkKey
  where checkKey (-1) = Nothing
        checkKey x = Just (fromIntegral x)
