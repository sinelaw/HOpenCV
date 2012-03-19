module Main where

import AI.CV.OpenCV
import Graphics.UI.Gtk
import Data.Maybe (isNothing)

showFrame :: Capture -> IplImage -> Image -> IO Bool
showFrame cap target image
  = do frame <- queryFrame cap

       convertImage frame target 0
       canny target target 30 190 3

       pb <- toGtkPixbuf target
       imageSetFromPixbuf image pb

       k <- waitKey 30
       return $ isNothing k

main :: IO ()
main
  = do _ <- initGUI
       win    <- windowNew
       set win [windowTitle := "OpenCV with Gtk"]

       cap    <- createCameraCapture pickAnyCam
       frame  <- queryFrame cap

       -- setup space for cannied frame and gtk image
       size   <- getSize frame
       target <- createImage size 1 iplDepth8u
       pixbuf <- toGtkPixbuf target
       image  <- imageNewFromPixbuf pixbuf
       containerAdd win image

       _ <- timeoutAdd (showFrame cap target image) 33

       widgetShowAll win
       mainGUI
