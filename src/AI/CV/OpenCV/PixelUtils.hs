{-# LANGUAGE BangPatterns #-}
-- |Images obtained from OpenCV usually have the components of color
-- pixels arranged in BGR order and pad image rows with unused
-- bytes. This module provides mechanisms to re-order pixels to RGB
-- and to drop the unused packing bytes.
module AI.CV.OpenCV.PixelUtils where
import AI.CV.OpenCV.HIplImage
import Control.Monad.ST (runST)
import Data.Word (Word8)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Generic as VG

-- |Generate indices to convert OpenCV-native BGR pixel ordering to
-- RGB and drop unused packing bytes from each row. The returned index
-- 'V.Vector' may be passed to 'toRGB'' to speed up pixel ordering
-- conversion when multiple conversions are to be performed.
rgbIndices :: Int -> Int -> Int -> V.Vector Int
rgbIndices width' stride numElems = V.fromList $ concatMap row rowStarts
    where rowStarts = [0,stride..numElems-1]
          row s = concatMap (\c -> map (s + c*3 +) [2,1,0]) [0..width'-1]

-- |Convert an 'HIplImage' \'s pixel data from BGR triplets in padded rows
-- to tightly packed rows of RGB pixels.
toRGB :: HIplImage -> V.Vector Word8
toRGB img = V.backpermute (pixels img) $
            rgbIndices (width img) (widthStep img) (imageSize img)

-- |Convert an 'HIplImage' \'s pixel data from BGR triplets in padded
-- rows to tightly packed rows of RGB pixels using the given
-- 'V.Vector' of indices. The index 'Vector' will typically be the
-- result of a previous call to 'rgbIndices'.
toRGB' :: HIplImage -> V.Vector Int -> V.Vector Word8
toRGB' img inds = V.backpermute (pixels img) inds

-- |Drop any pixels beyond real image data on each row.
dropAlpha :: V.Storable a => Int -> V.Vector a -> V.Vector a
dropAlpha w = V.ifilter (\i _ -> (i `rem` rowLength) < realWidth)
    where rowLength = w * 4
          realWidth = w * 3

-- |Return a Vector of bytes of a single color channel from a
-- tri-chromatic image. The desired channel must be one of 0, 1, or 2.
isolateChannel :: Int -> HIplImage -> V.Vector Word8
isolateChannel ch img = 
    if ch >= 3 || numCh /= 3
    then error $ "Invalid channel "++show ch++" for image with "++show numCh++
                 " color channels"
    else runST $ do v <- VM.new (w*h)
                    let go !x !p !p3 !y
                            | y >= h = VG.unsafeFreeze v
                            | x == w = go 0 p (p3+margin) (y+1)
                            | otherwise = do VM.unsafeWrite v p (get p3)
                                             go (x+1) (p+1) (p3+3) y
                    go 0 0 ch 0
    where numCh = numChannels img
          w = width img
          h = height img
          margin = widthStep img - (w  * 3)
          pix = pixels img
          get = V.unsafeIndex pix

-- |Convert an 'HIplImage' \'s pixel data to a 'V.Vector' of monochromatic bytes.
toMono :: HIplImage -> V.Vector Word8
toMono img = if numChannels img == 1 then dropAlpha w pix 
             else runST $ do v <- VM.new (w*h)
                             let go !x !p !p3 !y
                                     | y >= h = VG.unsafeFreeze v
                                     | x == w = go 0 p (p3+margin) (y+1)
                                     | otherwise = let grey = getAvg p3
                                                   in do VM.unsafeWrite v p grey
                                                         go (x+1) (p+1) (p3+3) y
                             go 0 0 0 0
    where w = width img
          h = height img
          margin = widthStep img - (w * 3)
          pix = pixels img
          get :: Int -> Int
          get = fromIntegral . V.unsafeIndex pix
          getAvg i = avg (get i) (get (i+1)) (get (i+2))
          avg :: Int -> Int -> Int -> Word8
          avg b g r = fromIntegral $ (b + g + r) `div` 3
