-- |Type-safe color conversion functions. 
module AI.CV.OpenCV.ColorConversion
    (convertGrayToRGB, convertGrayToBGR, 
     convertBGRToGray, convertRGBToGray,
     convertBayerBgToBGR, convertBayerBgToRGB,
     convertRGBToHSV, convertBGRToHSV, convertHSVToBGR) where
import AI.CV.OpenCV.Core.CV
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.ColorConversion
import AI.CV.OpenCV.Core.CVOp

convertGrayToRGB :: HasDepth d =>
                    HIplImage MonoChromatic d -> HIplImage TriChromatic d
convertGrayToRGB = convertColor cv_GRAY2RGB
{-# INLINE convertGrayToRGB #-}

convertGrayToBGR :: HasDepth d =>
                    HIplImage MonoChromatic d -> HIplImage TriChromatic d
convertGrayToBGR = convertColor cv_GRAY2BGR
{-# INLINE convertGrayToBGR #-}

convertBGRToGray :: HasDepth d =>
                    HIplImage TriChromatic d -> HIplImage MonoChromatic d
convertBGRToGray = convertColor cv_BGR2GRAY
{-# INLINE convertBGRToGray #-}

convertRGBToGray :: HasDepth d =>
                    HIplImage TriChromatic d -> HIplImage MonoChromatic d
convertRGBToGray = convertBGRToGray
{-# INLINE convertRGBToGray #-}

convertBayerBgToBGR :: HasDepth d =>
                       HIplImage MonoChromatic d -> HIplImage TriChromatic d
convertBayerBgToBGR = convertColor cv_BayerBG2BGR
{-# INLINE convertBayerBgToBGR #-}

convertBayerBgToRGB :: HasDepth d =>
                       HIplImage MonoChromatic d -> HIplImage TriChromatic d
convertBayerBgToRGB = convertColor cv_BayerBG2RGB
{-# INLINE convertBayerBgToRGB #-}

convertRGBToHSV :: HasDepth d =>
                   HIplImage TriChromatic d -> HIplImage TriChromatic d
convertRGBToHSV = convertColor cv_RGB2HSV
{-# INLINE convertRGBToHSV #-}

convertBGRToHSV :: HasDepth d =>
                   HIplImage TriChromatic d -> HIplImage TriChromatic d
convertBGRToHSV = convertColor cv_BGR2HSV
{-# INLINE convertBGRToHSV #-}

convertHSVToBGR :: HasDepth d =>
                   HIplImage TriChromatic d -> HIplImage TriChromatic d
convertHSVToBGR = convertColor cv_HSV2BGR
{-# INLINE convertHSVToBGR #-}

-- |Convert the color model of an image.
convertColor :: (HasChannels c1, HasChannels c2, HasDepth d) =>
                ColorConversion -> HIplImage c1 d -> HIplImage c2 d
convertColor cc = cv2 $ \src dst -> cvCvtColor src dst cc
-- convertColor cc img = unsafePerformIO . withHIplImage img $
--                         \src -> do dst <- mkHIplImage w h
--                                    withHIplImage dst $
--                                      \dst' -> cvCvtColor src dst' cc
--                                    return dst
--     where w = width img
--           h = height img
{-# INLINE convertColor #-}