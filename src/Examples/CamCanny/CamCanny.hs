import AI.CV.OpenCV.HighCV
import AI.CV.OpenCV.ArrayOps
import AI.CV.OpenCV.Filtering
import Control.Applicative
import Control.Parallel
import Rate (trackRate)

-- Just real-time edges
main2 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc = canny 70 110 3 . convertRGBToGray

-- Edges saved to file
main2a = do write <- createVideoWriter "hcv-edges.mp4" mpeg4CC 15 (640,480)
            cam <- createCameraCapture (Just 0)
            runWindow $ proc <$> cam >>= 
                        (>>) <$> write . convertGrayToBGR <*> return
    where proc = canny 70 110 3 . convertRGBToGray

-- Thick red edges added to raw video
main1 = createCameraCapture (Just 0) >>= runWindow . fmap (add <$> id <*> proc)
    where proc = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB 
               . canny 50 90 3 . convertRGBToGray


-- Thick red edges added to smoothed video (parallelism!).
main3 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc x = let e = edges x; s = smooth x
                   in e `par` s `pseq` add e s
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 50 90 3 . convertRGBToGray
          smooth = smoothGaussian 21

-- A very low-quality unsharp mask.
main5 = createCameraCapture (Just 0) >>= runWindow . fmap proc
    where proc x = let g = convertRGBToGray x :: GrayImage
                   in halvsies (convertGrayToRGB . normalize cv_MinMax 200 50 $ g)
                               (convertGrayToRGB . contrastBoost $ g) 
    --where proc x = halvsies (sub x . sub x . smoothGaussian 5 $ x) x
    -- where proc x = let d = convertScale 5 0 (absDiff x (smoothGaussian 5 x))
    --                    m = thresholdBinary 50 255 (convertRGBToGray d)
    --               in halvsies (subMask d m x) x

contrastBoost :: GrayImage -> GrayImage
contrastBoost = normalize cv_MinMax 255 0 
              . thresholdTruncate (200::Word8)
              . thresholdToZero 20

halvsies :: ColorImage -> ColorImage -> ColorImage
halvsies l r = cvOr l' r'
    where l' = resetROI . set (0,0,0) . setROI (CvRect 320 0 320 480) $ l
          r' = resetROI . set (0,0,0) . setROI (CvRect 0 0 320 480) $ r

-- Thick red edges added to smoothed video with framerate display.
main6 = do rater <- trackRate
           cam <- createCameraCapture (Just 0)
           runWindow $ proc <$> rater <*> cam
    where proc msg x = let e = edges x; s = smooth x
                       in e `par` s `pseq` showFPS msg (add e s)
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 70 110 3 . convertRGBToGray
          smooth = smoothGaussian 21
          showFPS = putText (300,450) (0,255,0) . (++" FPS")

-- Thick red edges added to smoothed video with framerate displayed in
-- a customized font.
main7 = do rater <- trackRate
           cam <- createCameraCapture (Just 0)
           str <- prepFont ComplexSerif False 1 1 2
           let str' = str (300,450) (0,255,0) . (++ " FPS")
           runWindow $ proc . str' <$> rater <*> cam
    where proc msg x = let e = edges x; s = smooth x
                       in e `par` s `pseq` msg (add e s)
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 70 110 3 . convertRGBToGray
          smooth = smoothGaussian 21

-- Thick red edges added to smoothed video with framerate displayed in
-- a customized font saved to a video file.
main8 = do rater <- trackRate
           cam <- createCameraCapture (Just 0)
           str <- prepFont ComplexSerif False 1 1 2
           let str' = str (300,450) (0,255,0) . (++ " FPS")
           write <- createVideoWriter "hcv.mp4" mpeg4CC 15 (640,480)
           runWindow $ do msg <- str' <$> rater
                          img <- proc msg <$> cam
                          (write *> return) img
    where proc msg x = let e = edges x; s = smooth x
                       in e `par` s `pseq` msg (add e s)
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 70 110 3 . convertRGBToGray
          smooth = smoothGaussian 21

-- Thick red edges added to smoothed video with framerate displayed in
-- a customized font saved to a video file. Terse code.
main9 = do rater <- trackRate
           cam <- createCameraCapture (Just 0)
           str <- prepFont ComplexSerif False 1 1 2
           let str' = str (300,450) (0,255,0) . (++ " FPS")
           write <- createVideoWriter "hcv.mp4" mpeg4CC 15 (640,480)
           runWindow $ (proc . str' <$> rater) <*> cam >>= 
                       ((>>) <$> write <*> return)
    where proc msg x = let e = edges x; s = smooth x
                       in e `par` s `pseq` msg (add e s)
          edges = dilate 1 . cvAndS (0,0,255) . convertGrayToRGB
                . canny 70 110 3 . convertRGBToGray
          smooth = smoothGaussian 21

-- Two-tone blueprint video.
main10 = do rater <- trackRate
            cam <- createCameraCapture (Just 0)
            str <- prepFont ComplexSerif False 1 1 2
            let str' = str (300,450) (0,255,0) . (++ " FPS")
            runWindow $ proc . str' <$> rater <*> cam
    where proc msg x = 
              let g = convertRGBToGray x
                  t = erode 4 . dilate 4 . thresholdBinaryOtsu 255 $ g
                  g' = let light = cvAndS (255,0,0) . convertGrayToRGB $ t
                           dark = cvAndS (180,0,0) . convertGrayToRGB 
                                . thresholdBinaryInv 100 255 $ t
                       in cvOr light dark
                  neon = smoothGaussian 3 . dilate 1 . convertGrayToRGB 
                       . canny 70 110 3 $ g
              in g' `par` neon `pseq` msg (add neon g')

-- Two-tone blueprint video save to video.
main11 = do rater <- trackRate
            cam <- createCameraCapture (Just 0)
            write <- createVideoWriter "blueprint.mp4" mpeg4CC 15 (640,480)
            str <- prepFont ComplexSerif False 1 1 2
            let str' = str (300,450) (0,255,0) . (++ " FPS")
            runWindow $ proc . str' <$> rater <*> cam >>=
                        (>>) <$> write <*> return
    where proc msg x = 
              let g = convertRGBToGray x
                  t = erode 4 . dilate 4 . thresholdBinaryOtsu 255 $ g
                  g' = let light = cvAndS (255,0,0) . convertGrayToRGB $ t
                           dark = cvAndS (180,0,0) . convertGrayToRGB 
                                . thresholdBinaryInv 100 255 $ t
                       in cvOr light dark
                  neon = smoothGaussian 3 . dilate 1 . convertGrayToRGB 
                       . canny 70 110 3 $ g
              in g' `par` neon `pseq` msg (add neon g')

main = main10
