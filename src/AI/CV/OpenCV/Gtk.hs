module AI.CV.OpenCV.Gtk
where

import AI.CV.OpenCV.CxCore
import Graphics.UI.Gtk

toGtkPixbuf :: IplImage -> IO Pixbuf
toGtkPixbuf i
  = do imageData <- getImageData i
       size      <- getSize i
       rowStride <- getWidthStep i
       pixbufNewFromData
         imageData
         ColorspaceRgb
         noAlphaChannel
         (fromIntegral 8)
         (fromIntegral . sizeWidth  $ size)
         (fromIntegral . sizeHeight $ size)
         (fromIntegral rowStride)
 where
  noAlphaChannel = False
