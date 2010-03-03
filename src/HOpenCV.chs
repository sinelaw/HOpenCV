{-# LANGUAGE ForeignFunctionInterface #-}

#include "HOpenCV.h"

module HOpenCV where 

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import C2HS

data IplImage = IplImage
data CvCapture = CvCapture

{#pointer *IplImage as CIplImage foreign -> IplImage #}
{#pointer *CvCapture as CCvCapture foreign -> CvCapture #}


foreign import ccall "HOpenCV.h &del_capture"
  p_del_capture :: FunPtr (Ptr CvCapture -> IO ())
foreign import ccall "HOpenCV.h &del_image"
  p_del_image :: FunPtr (Ptr IplImage -> IO ())

newCapture :: Int -> IO CCvCapture
newCapture deviceNum = do
  ptr <- {#call unsafe new_capture#} (cIntConv deviceNum)
  newForeignPtr p_del_capture ptr

--{#fun new_capture as ^
--      {fromIntegral `Int'} -> `Ptr CvCapture' id#}

--{#fun del_capture as ^
--      {id `Ptr CvCapture'} -> `()' id#}

queryClonedFrame :: CCvCapture -> IO CIplImage
queryClonedFrame capture = do
  ptr <- withForeignPtr capture $ \capture' -> {#call unsafe query_cloned_frame#} capture'
  newForeignPtr p_del_image ptr

  
--{#fun query_frame as ^
--      {id `Ptr CvCapture'} -> `Ptr IplImage' id#}

{#fun new_window as ^
      {fromIntegral `Int', fromIntegral `Int'} -> `()' id#}

{#fun del_window as ^
      {fromIntegral `Int'} -> `()' id#}

{#fun show_image as ^
      {fromIntegral `Int', unsafeForeignPtrToPtr `CIplImage'} -> `()' id#}

{#fun wait_key as ^
      {fromIntegral `Int'} -> `()' id#}

--{#fun clone_image as ^
--      {id `Ptr IplImage'} -> `Ptr IplImage' id#}
cloneImage :: CIplImage -> IO CIplImage
cloneImage image = do
  ptr <- withForeignPtr image $ \image' -> {#call unsafe clone_image#} image'
  newForeignPtr p_del_image ptr
  
--{#fun del_image as ^
--      {id `Ptr IplImage'} -> `()' id#}

--{#fun dilate as ^
--      {id `CIplImage', fromIntegral `Int', id `CIplImage'} -> `()' id#}
    

