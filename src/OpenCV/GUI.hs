-- |Very simple tools for showing images in a window. The 'runWindow'
-- and 'runNamedWindow' interfaces are the recommended entrypoints.
module OpenCV.GUI (namedWindow, WindowFlag(..), MouseCallback, 
                   waitKey, cvInit, runWindow, runNamedWindow) where
import OpenCV.Core.Image
import OpenCV.Core.HighGui
import OpenCV.Core.CxCore (fromArr)
import Control.Monad ((>=>))
import Data.Bits ((.&.))
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.C.String (newCString)

bool :: a -> a -> Bool -> a
bool t _ True = t
bool _ f False = f

-- |Simple window runner. Takes an action that produces images to be
-- shown in the window. Exits when any key is pressed.
runWindow :: IO (Image c Word8 r) -> IO ()
runWindow mkImg = newWindow 0 True >> go
    where go = do mkImg >>= flip withIplImage (showImage 0)
                  cvWaitKey 1 >>= bool (delWindow 0) go . (> 0)

-- |Simple named window runner. Exits when any key is pressed. The
-- name is shown in the window's title bar.
runNamedWindow :: String -> IO (Image c Word8 r) -> IO ()
runNamedWindow name mkImg = 
    do name' <- newCString name
       cvNamedWindow name' (windowFlagsToEnum [AutoSize])
       let showImg = cvShowImage name' . castPtr
           go = do mkImg >>= flip withIplImage showImg
                   cvWaitKey 1 >>= bool (cvDestroyWindow name') go . (> 0)
       go

-- |Create a new window with the given title. The return value is an
-- action for showing an image, and an action for destroying the
-- window. Be sure to repeatedly invoke 'waitKey' to keep the system
-- alive.
namedWindow :: String -> [WindowFlag] -> 
               --Maybe MouseCallback -> 
               IO (Image c d r -> IO (), IO ())
namedWindow name flags =
  do cstr <- newCString name
     let showImg img = withIplImage img $ \imgPtr ->
                         cvShowImage cstr (fromArr imgPtr)
     cvNamedWindow cstr (windowFlagsToEnum flags)
     return (showImg, cvDestroyWindow cstr)

-- | @waitKey delay@ waits for a key indifinitely if @delay <= 0@, or
-- for @delay@ milliseconds. The returned value is the code of the
-- pressed key or 'Nothing'.
waitKey :: Int -> IO (Maybe Int)
waitKey = cvWaitKey . fromIntegral >=> return . checkKey
  where checkKey (-1) = Nothing
        checkKey x = Just (fromIntegral (x .&. 0xFF))
