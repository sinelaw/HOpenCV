import AI.CV.OpenCV.HighCV
import AI.CV.OpenCV.ArrayOps
import AI.CV.OpenCV.Filtering
import Control.Applicative
import Control.Parallel

main2 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc = canny 50 90 3 . convertRGBToGray

main1 = createCameraCapture (Just 0) >>= runWindow . fmap (cvAdd <$> id <*> proc)
    where proc = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB 
               . canny 50 90 3 . convertRGBToGray

main = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc x = let e = edges x 
                       s = smooth x
                   in e `par` s `pseq` cvAdd e s
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 50 90 3 . convertRGBToGray
          smooth = smoothGaussian 25
