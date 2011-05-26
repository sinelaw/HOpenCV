import AI.CV.OpenCV.HighCV
import AI.CV.OpenCV.ArrayOps
import AI.CV.OpenCV.Filtering
import Control.Applicative
import Control.Parallel
import Data.IORef
import Text.Printf
import Data.Time.Clock

-- Just real-time edges
main2 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc = canny 50 90 3 . convertRGBToGray

-- Thick red edges added to raw video
main1 = createCameraCapture (Just 0) >>= runWindow . fmap (cvAdd <$> id <*> proc)
    where proc = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB 
               . canny 50 90 3 . convertRGBToGray

trackRate :: IO (IO String)
trackRate = do numFrames <- newIORef 0
               oldRate <- newIORef ""
               startTime <- getCurrentTime >>= newIORef
               return $ do n <- readIORef numFrames
                           if n == 30 then
                             do t <- getCurrentTime
                                s <- readIORef startTime
                                let dt = realToFrac $ diffUTCTime t s :: Float
                                    msg = printf "%.2f" (30.0 / dt)
                                writeIORef startTime t
                                writeIORef numFrames 0
                                writeIORef oldRate msg
                                return msg
                           else
                             do writeIORef numFrames (n+1)
                                readIORef oldRate

-- Thick red edges added to smoothed video (parallelism!).
main3 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc x = let e = edges x 
                       s = smooth x
                   in e `par` s `pseq` cvAdd e s
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 50 90 3 . convertRGBToGray
          smooth = smoothGaussian 25

-- Thick red edges added to smoothed video with framerate display.
main = do rater <- trackRate
          cam <- createCameraCapture (Just 0)
          runWindow $ do msg <- rater
                         proc msg <$> cam
    where proc msg x = let e = edges x 
                           s = smooth x
                       in e `par` s `pseq` showFPS msg (cvAdd e s)
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 50 90 3 . convertRGBToGray
          smooth = smoothGaussian 25
          showFPS s = putText (s++" FPS") (300,450) (0,255,0)
