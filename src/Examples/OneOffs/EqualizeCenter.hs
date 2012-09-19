import Control.Applicative
import OpenCV.HighCV
import OpenCV.ArrayOps
import OpenCV.Histograms

boostSaturation :: ColorImage -> ColorImage
boostSaturation img = convertHSVToBGR $ replaceChannel 1 s hsv
  where hsv = convertBGRToHSV img
        s = withROI (CvRect 100 100 300 300) (convertScale 2 0) $ isolateChannel 1 hsv

main = fromFileColor "../PerfTest/lena.jpg" >>=
       toFile "eq.png" . boostSaturation
