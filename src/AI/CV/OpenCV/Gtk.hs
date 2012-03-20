module AI.CV.OpenCV.Gtk
where

import AI.CV.OpenCV.CxCore
import Graphics.UI.Gtk

toGtkPixbuf :: IplImage -> IO Pixbuf
toGtkPixbuf i
  = do imageData <- getImageData i
       size      <- getSize i
       rowStride <- getWidthStep i
       d         <- getDepth i
       let depth = numBits d
       pixbufNewFromData
         imageData
         ColorspaceRgb
         noAlphaChannel
         (fromIntegral depth)
         (fromIntegral . sizeWidth  $ size)
         (fromIntegral . sizeHeight $ size)
         rowStride
 where
  noAlphaChannel = False
