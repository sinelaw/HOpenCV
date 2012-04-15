{-# Language EmptyDataDecls,ForeignFunctionInterface #-}

module AI.CV.OpenCV.Gtk where

import AI.CV.OpenCV.CxCore

import Graphics.UI.Gtk

toGtkPixbuf :: IplImage -> IO Pixbuf
toGtkPixbuf im
  = do imageData <- getImageData im
       size      <- getSize im
       rowStride <- getWidthStep im
       pixbufNewFromData
         imageData
         ColorspaceRgb
         noAlphaChannel
         (depth 8)
         (fromIntegral . sizeWidth  $ size)
         (fromIntegral . sizeHeight $ size)
         rowStride
 where
  noAlphaChannel = False
  depth i        = i
