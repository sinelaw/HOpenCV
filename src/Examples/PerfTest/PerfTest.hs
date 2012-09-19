{-# LANGUAGE TypeSynonymInstances #-}
import OpenCV.HighCV
import OpenCV.ArrayOps
import OpenCV.Filtering
import Control.Parallel
import Criterion.Main

-- Morphological closing
close :: GrayImage -> GrayImage
close = erode 4 . dilate 4
{-# INLINE close #-}

-- Posterize into four shades of blue.
fourTones :: GrayImage -> ColorImage
fourTones g = cvOr light dark
    where t = close . thresholdBinaryOtsu 255 $ g
          lightMean = avgMask g t
          l1 = close $ cmpS CmpGT lightMean g
          l2 = convertGrayToRGB $ cvNot l1 `cvAnd` t
          light = cvAndS (255,0,0) (convertGrayToRGB l1) `cvOr` 
                  cvAndS (220,0,0) l2
          t' = cvNot t
          darkMean = avgMask g t'
          d2 = close $ cmpS CmpLT darkMean g
          d1 = convertGrayToRGB $ cvNot d2 `cvAnd` t'
          dark = cvAndS (180,0,0) d1 `cvOr`
                 cvAndS (140,0,0) (convertGrayToRGB d2)
{-# INLINE fourTones #-}

-- Smoothed Canny edges.
neonEdges :: GrayImage -> ColorImage
neonEdges = convertGrayToRGB . smoothGaussian 5. dilate 1 . canny 70 110 3
{-# INLINE neonEdges #-}

-- A blueprint effect.
blueprint :: ColorImage -> ColorImage
blueprint x = toned `par` neon `pseq` add neon toned
    where g = convertRGBToGray x
          toned = fourTones g
          neon = neonEdges g
{-# INLINE blueprint #-}

-- No parallelism.
blueprintSlow :: ColorImage -> ColorImage
blueprintSlow x = add (fourTones g) (neonEdges g)
    where g = convertRGBToGray x
{-# INLINE blueprintSlow #-}

main :: IO ()
main = do img <- fromFile "lena.jpg"
          defaultMain [ 
              bench "blueprint" $ whnf blueprint img
            , bench "blueprintSlow" $ whnf blueprintSlow img ]
