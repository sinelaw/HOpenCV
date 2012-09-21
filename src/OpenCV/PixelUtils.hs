{-# LANGUAGE BangPatterns, DataKinds #-}
-- |Images obtained from OpenCV usually have the components of color
-- pixels arranged in BGR order and pad image rows with unused
-- bytes. This module provides mechanisms to drop the unused packing
-- bytes.
module OpenCV.PixelUtils where
import OpenCV.Core.Image
import OpenCV.Core.ImageUtil
import OpenCV.ColorConversion (convertRGBToGray)
import Control.Monad.ST (runST)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Generic as VG
import Unsafe.Coerce (unsafeCoerce)

-- |OpenCV often stores color images with rows that are four times the
-- width (which aligns rows nicely, and can accommodate RGBA
-- pixels). For network transmission, it can be advantageous to strip
-- out that extra data. This function returns a fresh 'V.Vector' of
-- pixel data that excludes these unused bytes. If the original image
-- data is already packed, it is returned as a 'V.Vector' without
-- copying.
packPixels :: Image c d NoROI -> V.Vector d
packPixels img@Image{} = 
    if w' == stride 
    then pixelVector img
    else runST $ do v <- VM.new (w*h*nc)
                    let sliceSrc x = V.unsafeSlice x w' pix
                        sliceDst x = VM.unsafeSlice x w' v
                        go !y !pSrc !pDst
                            | y < h = let s1 = sliceSrc pSrc
                                          s2 = sliceDst pDst
                                          pSrc' = pSrc + stride
                                          pDst' = pDst + w'
                                      in do V.unsafeCopy s2 s1
                                            go (y+1) pSrc' pDst'
                            | otherwise = VG.unsafeFreeze v
                    go 0 0 0
    where w = fromIntegral $ width img
          h = fromIntegral $ height img
          nc = imgChannels img
          w' = w * nc
          stride = fromIntegral $ widthStep img
          pix = pixelVector img
{-# INLINE packPixels #-}

-- |Return a Vector of bytes of a single color channel from a
-- tri-chromatic image. The desired channel must be one of 0, 1, or 2.
isolateChannel :: Int -> Image Trichromatic d NoROI -> V.Vector d
isolateChannel ch img@Image{} = 
    if ch < 0 || ch >= 3
    then error $ "Invalid channel "++show ch++" for trichromatic image"
    else runST $ do v <- VM.new (w*h)
                    let go !x !p !p3 !y
                            | y >= h = VG.unsafeFreeze v
                            | x == w = go 0 p (p3+margin) (y+1)
                            | otherwise = do VM.unsafeWrite v p (get p3)
                                             go (x+1) (p+1) (p3+3) y
                    go 0 0 ch 0
    where w = fromIntegral $ width img
          h = fromIntegral $ height img
          margin = fromIntegral (widthStep img) - (w  * 3)
          pix = pixelVector img
          get = V.unsafeIndex pix
{-# INLINE isolateChannel #-}

-- |Convert an 'Image' \'s pixel data to a 'V.Vector' of monochromatic bytes.
toMono :: Integral d => Image c d NoROI -> V.Vector d
toMono img@Image{} = if imgChannels img == 1 then packPixels img
                     else packPixels . convertRGBToGray . isColor $ 
                          unsafeCoerce img

