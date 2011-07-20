{-# LANGUAGE FlexibleContexts #-}
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

type M = MonoChromatic
type T = TriChromatic

convertGrayToRGB :: (HasDepth d, InplaceROI r M d T d) =>
                    HIplImage MonoChromatic d r -> HIplImage TriChromatic d r
convertGrayToRGB = convertColor cv_GRAY2RGB
{-# INLINE convertGrayToRGB #-}

convertGrayToBGR :: (HasDepth d, InplaceROI r M d T d) =>
                    HIplImage MonoChromatic d r -> HIplImage TriChromatic d r
convertGrayToBGR = convertColor cv_GRAY2BGR
{-# INLINE convertGrayToBGR #-}

convertBGRToGray :: (HasDepth d, InplaceROI r T d M d) =>
                    HIplImage TriChromatic d r -> HIplImage MonoChromatic d r
convertBGRToGray = convertColor cv_BGR2GRAY
{-# INLINE convertBGRToGray #-}

convertRGBToGray :: (HasDepth d, InplaceROI r T d M d) =>
                    HIplImage TriChromatic d r -> HIplImage MonoChromatic d r
convertRGBToGray = convertBGRToGray
{-# INLINE convertRGBToGray #-}

convertBayerBgToBGR :: (HasDepth d, InplaceROI r M d T d) =>
                       HIplImage MonoChromatic d r -> HIplImage TriChromatic d r
convertBayerBgToBGR = convertColor cv_BayerBG2BGR
{-# INLINE convertBayerBgToBGR #-}

convertBayerBgToRGB :: (HasDepth d, InplaceROI r M d T d) =>
                       HIplImage MonoChromatic d r -> HIplImage TriChromatic d r
convertBayerBgToRGB = convertColor cv_BayerBG2RGB
{-# INLINE convertBayerBgToRGB #-}

convertRGBToHSV :: (HasDepth d, InplaceROI r T d T d) =>
                   HIplImage TriChromatic d r -> HIplImage TriChromatic d r
convertRGBToHSV = convertColor cv_RGB2HSV
{-# INLINE convertRGBToHSV #-}

convertBGRToHSV :: (HasDepth d, InplaceROI r T d T d) =>
                   HIplImage TriChromatic d r -> HIplImage TriChromatic d r
convertBGRToHSV = convertColor cv_BGR2HSV
{-# INLINE convertBGRToHSV #-}

convertHSVToBGR :: (HasDepth d, InplaceROI r T d T d) =>
                   HIplImage TriChromatic d r -> HIplImage TriChromatic d r
convertHSVToBGR = convertColor cv_HSV2BGR
{-# INLINE convertHSVToBGR #-}

-- |Convert the color model of an image.
convertColor :: (HasChannels c1, HasChannels c2, HasDepth d, 
                 InplaceROI r c1 d c2 d) =>
                ColorConversion -> HIplImage c1 d r -> HIplImage c2 d r
convertColor cc = cv2 $ \src dst -> cvCvtColor src dst cc
{-# INLINE convertColor #-}