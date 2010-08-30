-- |Type-safe color conversion functions. 
module AI.CV.OpenCV.ColorConversion
    (convertGrayToRGB, convertGrayToBGR, 
     convertBGRToGray, convertRGBToGray) where
import AI.CV.OpenCV.Core.CV
import AI.CV.OpenCV.Core.HIplUtils
import AI.CV.OpenCV.Core.ColorConversion
import Control.Monad.ST (runST, unsafeIOToST)

convertGrayToRGB :: HasDepth d =>
                    HIplImage a MonoChromatic d -> 
                    HIplImage FreshImage TriChromatic d
convertGrayToRGB = convertColor cv_GRAY2RGB

convertGrayToBGR :: HasDepth d =>
                    HIplImage a MonoChromatic d -> 
                    HIplImage FreshImage TriChromatic d
convertGrayToBGR = convertColor cv_GRAY2BGR

convertBGRToGray :: HasDepth d =>
                    HIplImage a TriChromatic d -> 
                    HIplImage FreshImage MonoChromatic d
convertBGRToGray = convertColor cv_BGR2GRAY

convertRGBToGray :: HasDepth d =>
                    HIplImage a TriChromatic d -> 
                    HIplImage FreshImage MonoChromatic d
convertRGBToGray = convertBGRToGray

-- |Convert the color model of an image.
convertColor :: (HasChannels c1, HasChannels c2, HasDepth d) =>
                ColorConversion -> HIplImage a c1 d -> HIplImage FreshImage c2 d
convertColor cc img = runST $ unsafeIOToST $ 
                      withHIplImage img $
                        \src -> do dst <- mkHIplImage w h
                                   withHIplImage dst $
                                     \dst' -> cvCvtColor src dst' cc
                                   return dst
    where w = width img
          h = height img
