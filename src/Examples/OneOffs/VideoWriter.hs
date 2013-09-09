-- | Save video from an attached webcam to compressed video on disk
-- while also showing it on-screen.
import OpenCV.HighCV
import System.Exit (exitSuccess)

main :: IO ()
main = do cam <- createCameraCapture Nothing :: IO (IO ColorImage)
          writeImg <- createVideoWriter "foo.avi" (toFourCC "XVID") 24 (640,480)
          (showImg,close) <- namedWindow "Video Test" [AutoSize]
          let kb 27 = close >> exitSuccess
              kb  _ = go
              go = do img <- cam
                      showImg img
                      writeImg img
                      waitKey 1 >>= maybe go kb
          go