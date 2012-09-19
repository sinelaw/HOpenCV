-- |An example application demonstrating realtime image processing on
-- the video feed from an attached webcam or a video file specified as
-- a command line argument. The executable prints usage instructions
-- to the console when run.
import OpenCV.HighCV
import OpenCV.ArrayOps
import OpenCV.Filtering
import OpenCV.Histograms
import Control.Applicative
import Control.Parallel
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Rate

-- Canny edges
edges = convertGrayToRGB . canny 70 110 3 . convertRGBToGray
{-# INLINE edges #-}

-- Heavily smoothed video with red edge highlights.
edgesOnSmoothed x = let e = edges x; s = smooth x in e `par` s `pseq` add e s
  where edges = cvAndS (0,0,255) . convertGrayToRGB . dilate 1
              . canny 70 110 3 . convertRGBToGray
        smooth = smoothGaussian 21
{-# INLINE edgesOnSmoothed #-}

-- Morphological closing
close :: GrayImage -> GrayImage
close = erode 4 . dilate 4
{-# INLINE close #-}

-- Posterize into two shades of blue.
twoTone :: GrayImage -> ColorImage
twoTone g = light t `cvOr` dark t
    where t = close . thresholdBinaryOtsu 255 $ g
          light = cvAndS (255,0,0) . convertGrayToRGB
          dark = cvAndS (180,0,0) . convertGrayToRGB . cvNot
{-# INLINE twoTone #-}

-- Smoothed Canny edges.
neonEdges :: GrayImage -> ColorImage
neonEdges = convertGrayToRGB . smoothGaussian 3 . dilate 1 . canny 70 110 3

-- Boost saturation
boostSat x = convertHSVToBGR $ replaceChannel 1 s' hsv
  where hsv = convertBGRToHSV x
        s' = convertScale 2.0 0 . isolateChannel 1 $ hsv
{-# INLINE boostSat #-}

-- Saturate and blur the borders
centralFocus :: ColorImage -> ColorImage
centralFocus img = withROI r (copy (setROI r img)) bg
  where bg = smoothGaussian 35 . boostSat $ img
        r = CvRect 150 100 340 280
{-# INLINE centralFocus #-}

-- A two-tone blueprint effect.
blueprint x = toned `par` neon `pseq` add neon toned
  where g = convertRGBToGray x
        toned = twoTone g
        neon = neonEdges g
{-# INLINE blueprint #-}

-- No parallelism
blueprintSlow x = add (neonEdges g) (twoTone g)
  where g = convertRGBToGray x
{-# INLINE blueprintSlow #-}

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

-- A four-tone blueprint effect.
blueprint2 x = toned `par` neon `pseq` add neon toned
  where g = convertRGBToGray x
        toned = fourTones g
        neon = neonEdges g
{-# INLINE blueprint2 #-}

-- No parallelism
blueprint2slow x = add (neonEdges g) (fourTones g)
  where g = convertRGBToGray x
{-# INLINE blueprint2slow #-}

-- NOTE: trackRate counts all the time in between frames. In low-light
-- situations, a camera may run at a lower rate to effect a longer
-- exposure time. To still report a useful performance metric, the
-- perfMon monitor counts only the time a frame is being processed and
-- drawn. Thus, the displayed framerate is the maximum theoretical
-- rate the processing and display code could run at if the image
-- capturing mechanism could feed it that fast.

main = do args <- getArgs
          cam <- case args of
                   ["--help"] -> do putStrLn "Usage: ./VideoFunhouse [filename]"
                                    putStr "If no file is given, a connected "
                                    putStrLn "camera is opened."
                                    exitSuccess
                   [fname] -> createFileCaptureLoop fname
                   _ -> createCameraCapture (Just 0)
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
              checkKey b _ 56 = go b boostSat        -- 8
              checkKey b _ 57 = go b centralFocus    -- 9
              checkKey b p 102 = go (not b) p
              checkKey _ _ 27 = close >> exitSuccess
              checkKey b p _ = go b p
              go False proc = cam >>= startFrame >>= showImg . proc >> 
                              stopFrame >> waitKey 1 >>= 
                              maybe (go False proc) (checkKey False proc)
              go True proc = cam >>= startFrame >>= (showFPS <*>) . pure . proc >>= 
                             showImg >> stopFrame >> waitKey 1 >>=
                             maybe (go True proc) (checkKey True proc)
              -- go False proc = cam >>= showImg . proc >> waitKey 1 >>=
              --                 maybe (go False proc) (checkKey False proc)
              -- go True proc = cam >>= (showFPS <*>) . pure . proc >>= showImg >>
              --                waitKey 1 >>= 
              --                maybe (go True proc) (checkKey True proc)
          showHelp
          go False id

showHelp :: IO ()
showHelp = do p "Usage: VideoFunhouse [file]"
              p ""
              p "Press 'f' to toggle framerate display"
              p "  The rate is computed from the per-frame processing time."
              p "  Lighting conditions and the specific camera used will"
              p "  determine the actual rate at which frames are acquired."
              p ""
              p "Number keys select a video effect:"
              p " 1 - Raw video"
              p " 2 - Canny edges"
              p " 3 - Smoothed image with red edge highlights"
              p " 4 - A two-tone blueprint effect"
              p " 5 - Two-tone blueprint effect without par annotations"
              p " 6 - A four-tone blueprint effect"
              p " 7 - Four-tone blueprint effect without par annotations"
              p " 8 - Saturation boost"
              p " 9 - Focus on the middle"
              p ""
              p "Press Esc to exit."
    where p = putStrLn
