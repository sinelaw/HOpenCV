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

type M = Monochromatic
type T = Trichromatic

convertGrayToRGB :: (HasDepth d, Inplace r M d T d) =>
                    HIplImage Monochromatic d r -> HIplImage Trichromatic d r
convertGrayToRGB = convertColor cv_GRAY2RGB
{-# INLINE convertGrayToRGB #-}

convertGrayToBGR :: (HasDepth d, Inplace r M d T d) =>
                    HIplImage Monochromatic d r -> HIplImage Trichromatic d r
convertGrayToBGR = convertColor cv_GRAY2BGR
{-# INLINE convertGrayToBGR #-}

convertBGRToGray :: (HasDepth d, Inplace r T d M d) =>
                    HIplImage Trichromatic d r -> HIplImage Monochromatic d r
convertBGRToGray = convertColor cv_BGR2GRAY
{-# INLINE convertBGRToGray #-}

convertRGBToGray :: (HasDepth d, Inplace r T d M d) =>
                    HIplImage Trichromatic d r -> HIplImage Monochromatic d r
convertRGBToGray = convertBGRToGray
{-# INLINE convertRGBToGray #-}

convertBayerBgToBGR :: (HasDepth d, Inplace r M d T d) =>
                       HIplImage Monochromatic d r -> HIplImage Trichromatic d r
convertBayerBgToBGR = convertColor cv_BayerBG2BGR
{-# INLINE convertBayerBgToBGR #-}

convertBayerBgToRGB :: (HasDepth d, Inplace r M d T d) =>
                       HIplImage Monochromatic d r -> HIplImage Trichromatic d r
convertBayerBgToRGB = convertColor cv_BayerBG2RGB
{-# INLINE convertBayerBgToRGB #-}

convertRGBToHSV :: (HasDepth d, Inplace r T d T d) =>
                   HIplImage Trichromatic d r -> HIplImage Trichromatic d r
convertRGBToHSV = convertColor cv_RGB2HSV
{-# INLINE convertRGBToHSV #-}

convertBGRToHSV :: (HasDepth d, Inplace r T d T d) =>
                   HIplImage Trichromatic d r -> HIplImage Trichromatic d r
convertBGRToHSV = convertColor cv_BGR2HSV
{-# INLINE convertBGRToHSV #-}

convertHSVToBGR :: (HasDepth d, Inplace r T d T d) =>
                   HIplImage Trichromatic d r -> HIplImage Trichromatic d r
convertHSVToBGR = convertColor cv_HSV2BGR
{-# INLINE convertHSVToBGR #-}

-- |Convert the color model of an image.
convertColor :: (HasChannels c1, HasChannels c2, HasDepth d, 
                 Inplace r c1 d c2 d) =>
                ColorConversion -> HIplImage c1 d r -> HIplImage c2 d r
convertColor cc = cv2 $ \src dst -> cvCvtColor src dst cc
{-# INLINE convertColor #-}