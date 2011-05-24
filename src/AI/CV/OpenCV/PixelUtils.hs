{-# LANGUAGE BangPatterns #-}
-- |Images obtained from OpenCV usually have the components of color
-- pixels arranged in BGR order and pad image rows with unused
-- bytes. This module provides mechanisms to drop the unused packing
-- bytes.
module AI.CV.OpenCV.PixelUtils where
import AI.CV.OpenCV.Core.HIplImage
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.ColorConversion (convertRGBToGray)
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
packPixels :: (HasChannels c, HasDepth d) => HIplImage c d -> V.Vector d
packPixels img = 
    if w' == stride 
    then pixels img
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
    where w = width img
          h = height img
          nc = imgChannels img
          w' = w * nc
          stride = widthStep img
          pix = pixels img
{-# INLINE packPixels #-}

-- |Return a Vector of bytes of a single color channel from a
-- tri-chromatic image. The desired channel must be one of 0, 1, or 2.
isolateChannel :: HasDepth d => Int -> HIplImage TriChromatic d -> V.Vector d
isolateChannel ch img = 
    if ch < 0 || ch >= 3
    then error $ "Invalid channel "++show ch++" for trichromatic image"
    else runST $ do v <- VM.new (w*h)
                    let go !x !p !p3 !y
                            | y >= h = VG.unsafeFreeze v
                            | x == w = go 0 p (p3+margin) (y+1)
                            | otherwise = do VM.unsafeWrite v p (get p3)
                                             go (x+1) (p+1) (p3+3) y
                    go 0 0 ch 0
    where w = width img
          h = height img
          margin = widthStep img - (w  * 3)
          pix = pixels img
          get = V.unsafeIndex pix
{-# INLINE isolateChannel #-}

-- |Convert an 'HIplImage' \'s pixel data to a 'V.Vector' of monochromatic bytes.
toMono :: (HasChannels c, HasDepth d, Integral d) => HIplImage c d -> V.Vector d
toMono img = if imgChannels img == 1 then packPixels img
             else packPixels . convertRGBToGray . isColor $ unsafeCoerce img

