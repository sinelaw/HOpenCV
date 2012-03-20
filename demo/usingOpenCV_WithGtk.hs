module Main where

import AI.CV.OpenCV
import Graphics.UI.Gtk
import Data.Maybe (isNothing)

-- AFAICT, a pixbuf is always three or four channels, depending 
-- on whether an alpha channel has been added to it. Since the
-- canny function only takes a single channel image it becomes 
-- neccesary to convert it to a three channel RGB before passing
-- it to toGtkPixbuf.

showFrame :: Capture -> IplImage -> IplImage -> Image -> IO Bool
showFrame cap cannyImage threeChannelImage gtkImage
  = do frame <- queryFrame cap

       convertImage frame cannyImage 0
       canny cannyImage cannyImage 30 190 3

       cvtColor cannyImage threeChannelImage GRAY2RGB
       pb <- toGtkPixbuf threeChannelImage
       imageSetFromPixbuf gtkImage pb
 
       k <- waitKey 5 -- for some reason an image won't get 
                      -- displayed unless this is here
       return $ isNothing k

main :: IO ()
main
  = do _ <- initGUI
       win   <- windowNew
       set win [windowTitle := "OpenCV with Gtk"]
       _ <- on win objectDestroy mainQuit

       cap   <- createCameraCapture pickAnyCam
       frame <- queryFrame cap

       -- setup space for cannied frame and gtk image
       size <- getSize frame
       cannyImage      <- createImage size 1 iplDepth8u
       backTo3Channels <- createImage size 3 iplDepth8u
       gtkImage        <- imageNew
       containerAdd win gtkImage

       _ <- timeoutAdd (showFrame cap cannyImage
                                      backTo3Channels
                                      gtkImage)
                       33

       widgetShowAll win
       mainGUI
