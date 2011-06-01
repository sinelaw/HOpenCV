{-# LANGUAGE FlexibleInstances #-}
import AI.CV.OpenCV.HighCV
import AI.CV.OpenCV.ArrayOps
import AI.CV.OpenCV.Filtering
import Control.Applicative
import Control.Parallel
import System.Exit (exitSuccess)
import Rate

edges = convertGrayToRGB . canny 70 110 3 . convertRGBToGray
{-# INLINE edges #-}

edgesOnSmoothed x = let e = edges x; s = smooth x in e `par` s `pseq` add e s
  where edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB 
              . canny 70 110 3 . convertRGBToGray
        smooth = smoothGaussian 21
{-# INLINE edgesOnSmoothed #-}

-- I find running with +RTS -N2 on a dual core CPU with
-- hyper-threading to be faster than -N. The presence of HT looks like
-- more cores, but they don't seem to be helping.
blueprint x = toned `par` neon `pseq` add neon toned
  where g = convertRGBToGray x
        t = erode 4 . dilate 4 . thresholdBinaryOtsu 255 $ g
        light = cvAndS (255,0,0) . convertGrayToRGB $ t
        dark = cvAndS (180,0,0) . convertGrayToRGB 
             . cvNot $ t
        toned = cvOr light dark
        neon = convertGrayToRGB . smoothGaussian 3 . dilate 1
             . canny 70 110 3 $ g
{-# INLINE blueprint #-}

close :: GrayImage -> GrayImage
close = erode 4 . dilate 4
{-# INLINE close #-}

-- No parallelism
blueprintSlow x = add neon toned
  where g = convertRGBToGray x
        t = erode 4 . dilate 4 . thresholdBinaryOtsu 255 $ g
        light = cvAndS (255,0,0) . convertGrayToRGB $ t
        dark = cvAndS (180,0,0) . convertGrayToRGB 
             . thresholdBinaryInv 100 255 $ t
        toned = cvOr light dark
        neon = convertGrayToRGB . smoothGaussian 3 . dilate 1
             . canny 70 110 3 $ g
{-# INLINE blueprintSlow #-}

-- Four blue tones.
blueprint2 x = toned `par` neon `pseq` add neon toned
  where g = convertRGBToGray x
        t = close . thresholdBinaryOtsu 255 $ g
        light = let lightMean = avgMask g t
                    l1 = close $ cmpS CmpGT lightMean g
                    l2 = convertGrayToRGB $ cvNot l1 `cvAnd` t
                in cvAndS (255,0,0) (convertGrayToRGB l1) `cvOr` 
                   cvAndS (220,0,0) l2
        -- light = cvAndS (255,0,0) . convertGrayToRGB $ t
        -- dark = cvAndS (180,0,0) . convertGrayToRGB 
        --      . thresholdBinaryInv 100 255 $ t
        dark = let t' = cvNot t
                   darkMean = avgMask g t'
                   d2 = close $ cmpS CmpLT darkMean g
                   d1 = convertGrayToRGB $ cvNot d2 `cvAnd` t'
               in cvAndS (180,0,0) d1 `cvOr`
                  cvAndS (140,0,0) (convertGrayToRGB d2)
        toned = cvOr light dark
        neon = convertGrayToRGB . smoothGaussian 3 . dilate 1
             . canny 70 110 3 $ g
{-# INLINE blueprint2 #-}

blueprint2slow x = add neon toned
  where g = convertRGBToGray x
        t = close . thresholdBinaryOtsu 255 $ g
        -- light = cvAndS (255,0,0) . convertGrayToRGB $ t
        light = let lightMean = avgMask g t
                    l1 = close $ cmpS CmpGT lightMean g
                    l2 = convertGrayToRGB $ cvNot l1 `cvAnd` t
                in cvAndS (255,0,0) (convertGrayToRGB l1) `cvOr` 
                   cvAndS (220,0,0) l2
        dark = let t' = cvNot t
                   darkMean = avgMask g t'
                   d2 = close $ cmpS CmpLT darkMean g
                   d1 = convertGrayToRGB $ cvNot d2 `cvAnd` t'
               in cvAndS (180,0,0) d1 `cvOr`
                  cvAndS (140,0,0) (convertGrayToRGB d2)
        toned = cvOr light dark
        neon = convertGrayToRGB . smoothGaussian 3 . dilate 1
             . canny 70 110 3 $ g
{-# INLINE blueprint2slow #-}


-- NOTE: trackRate counts all the time in between frames. In low-light
-- situations, a camera may run at a lower rate to effect a longer
-- exposure time. To still report a useful performance metric, the
-- perfMon monitor counts only the time a frame is being processed and
-- drawn.

main = do cam <- createCameraCapture (Just 0)
          (showImg,close) <- namedWindow "Video Funhouse" [AutoSize]
          --rater <- trackRate
          (startFrame', curr, stopFrame) <- perfMon
          str <- prepFont ComplexSerif False 1 1 2
          let showFPS :: IO (ColorImage -> ColorImage)
              --showFPS = str (300,450) (0,255,0) . (++ " FPS") <$> rater
              showFPS = str (300,450) (0,255,0) . (++ " FPS") <$> curr
              startFrame x = startFrame' >> return x
              checkKey b _ 49 = go b id              -- 1
              checkKey b _ 50 = go b edges           -- 2
              checkKey b _ 51 = go b edgesOnSmoothed -- 3
              checkKey b _ 52 = go b blueprint       -- 4 
              checkKey b _ 53 = go b blueprintSlow   -- 5
              checkKey b _ 54 = go b blueprint2      -- 6
              checkKey b _ 55 = go b blueprint2slow  -- 7
              checkKey b p 102 = go (not b) p
              checkKey _ _ 27 = close >> exitSuccess
              checkKey b p _ = go b p
              go False proc = cam >>= startFrame >>= showImg . proc >> stopFrame >> 
                              waitKey 1 >>= maybe (go False proc) (checkKey False proc)
              go True proc = cam >>= startFrame >>= (showFPS <*>) . pure . proc >>= 
                             showImg >> stopFrame >> waitKey 1 >>=
                             maybe (go True proc) (checkKey True proc)
              -- go False proc = cam >>= showImg . proc >> waitKey 1 >>=
              --                 maybe (go False proc) (checkKey False proc)
              -- go True proc = cam >>= (showFPS <*>) . pure . proc >>= showImg >>
              --                waitKey 1 >>= 
              --                maybe (go True proc) (checkKey True proc)
          go False id