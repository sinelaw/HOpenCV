{-# LANGUAGE FlexibleContexts, DataKinds #-}
-- |Type-safe color conversion functions. 
module OpenCV.ColorConversion
    (convertGrayToRGB, convertGrayToBGR, 
     convertBGRToGray, convertRGBToGray,
     convertBayerBgToBGR, convertBayerBgToRGB,
     convertRGBToHSV, convertBGRToHSV, convertHSVToBGR,
     convertColor) where
import OpenCV.Core.CV
import OpenCV.Core.ImageUtil
import OpenCV.Core.ColorConversion
import OpenCV.Core.CVOp

type M = Monochromatic
type T = Trichromatic

convertGrayToRGB :: (HasDepth d, Inplace r M d T d) =>
                    Image Monochromatic d r -> Image Trichromatic d r
convertGrayToRGB = convertColor cv_GRAY2RGB
{-# INLINE convertGrayToRGB #-}

convertGrayToBGR :: (HasDepth d, Inplace r M d T d) =>
                    Image Monochromatic d r -> Image Trichromatic d r
convertGrayToBGR = convertColor cv_GRAY2BGR
{-# INLINE convertGrayToBGR #-}

convertBGRToGray :: (HasDepth d, Inplace r T d M d) =>
                    Image Trichromatic d r -> Image Monochromatic d r
convertBGRToGray = convertColor cv_BGR2GRAY
{-# INLINE convertBGRToGray #-}

convertRGBToGray :: (HasDepth d, Inplace r T d M d) =>
                    Image Trichromatic d r -> Image Monochromatic d r
convertRGBToGray = convertBGRToGray
{-# INLINE convertRGBToGray #-}

convertBayerBgToBGR :: (HasDepth d, Inplace r M d T d) =>
                       Image Monochromatic d r -> Image Trichromatic d r
convertBayerBgToBGR = convertColor cv_BayerBG2BGR
{-# INLINE convertBayerBgToBGR #-}

convertBayerBgToRGB :: (HasDepth d, Inplace r M d T d) =>
                       Image Monochromatic d r -> Image Trichromatic d r
convertBayerBgToRGB = convertColor cv_BayerBG2RGB
{-# INLINE convertBayerBgToRGB #-}

convertRGBToHSV :: (HasDepth d, Inplace r T d T d) =>
                   Image Trichromatic d r -> Image Trichromatic d r
convertRGBToHSV = convertColor cv_RGB2HSV
{-# INLINE convertRGBToHSV #-}

convertBGRToHSV :: (HasDepth d, Inplace r T d T d) =>
                   Image Trichromatic d r -> Image Trichromatic d r
convertBGRToHSV = convertColor cv_BGR2HSV
{-# INLINE convertBGRToHSV #-}

convertHSVToBGR :: (HasDepth d, Inplace r T d T d) =>
                   Image Trichromatic d r -> Image Trichromatic d r
convertHSVToBGR = convertColor cv_HSV2BGR
{-# INLINE convertHSVToBGR #-}

-- |Convert the color model of an image.
convertColor :: (HasDepth d, Inplace r c1 d c2 d) =>
                ColorConversion -> Image c1 d r -> Image c2 d r
convertColor cc = cv2 $ \src dst -> cvCvtColor src dst cc
{-# INLINE convertColor #-}