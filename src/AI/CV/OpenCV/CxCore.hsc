{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CxCore where

import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.C.String
import Foreign


#include <cxcore.h>

------------------------------------------------------

data CvSize  = CvSize { sizeWidth :: CInt, sizeHeight :: CInt }
instance Storable CvSize where
    sizeOf    _ = (#size CvSize)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        w <- (#peek CvSize, width) ptr
        h <- (#peek CvSize, height) ptr
        return  (CvSize w h)
    poke ptr (CvSize w h) = do
        (#poke CvSize, width) ptr w
        (#poke CvSize, height) ptr h

data CvRect  = CvRect { rectX :: CInt, rectY :: CInt, rectWidth :: CInt, rectHeight :: CInt }
instance Storable CvRect where
    sizeOf    _ = (#size CvRect)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        x <- (#peek CvRect, x) ptr
        y <- (#peek CvRect, y) ptr
        w <- (#peek CvRect, width) ptr
        h <- (#peek CvRect, height) ptr
        return  (CvRect x y w h)
    poke ptr (CvRect x y w h) = do
        (#poke CvRect, x) ptr x
        (#poke CvRect, y) ptr y
        (#poke CvRect, width) ptr w
        (#poke CvRect, height) ptr h

------------------------------------------------------
class IplArrayType a

data CvArr
instance IplArrayType CvArr

data IplImage
instance IplArrayType IplImage

data CvMemStorage

data CvSeq a

fromArr :: IplArrayType a => Ptr a -> Ptr CvArr
fromArr = castPtr 

newtype Depth = Depth { unDepth :: CInt } 
    deriving (Eq, Show)
             
#{enum Depth, Depth             
  , iplDepth1u = IPL_DEPTH_1U
  , iplDepth8u = IPL_DEPTH_8U
  , iplDepth8s = IPL_DEPTH_8S
  , iplDepth16u = IPL_DEPTH_16U
  , iplDepth16s = IPL_DEPTH_16S
  , iplDepth32s = IPL_DEPTH_32S
  , iplDepth32f = IPL_DEPTH_32F
  , iplDepth64f = IPL_DEPTH_64F
}               

validDepths :: [Depth]
validDepths = [iplDepth1u, iplDepth8u, iplDepth8s, iplDepth16u, iplDepth16s, iplDepth32s, iplDepth32f, iplDepth64f]

depthsLookupList :: [(CInt, Depth)]
depthsLookupList = map (\d -> (unDepth d, d)) validDepths

numToDepth :: CInt -> Maybe Depth
numToDepth x = lookup x depthsLookupList
  

---------------------------------------------------------------
-- mem storage
foreign import ccall unsafe "cxcore.h cvCreateMemStorage"
  c_cvCreateMemStorage :: CInt -> IO (Ptr CvMemStorage)

cvCreateMemStorage :: CInt -> IO (Ptr CvMemStorage)
cvCreateMemStorage = errorName "Failed to create mem storage" . checkPtr . c_cvCreateMemStorage 

foreign import ccall unsafe "HOpenCV_warp.h release_mem_storage"
  cvReleaseMemStorage :: Ptr CvMemStorage -> IO ()

foreign import ccall unsafe "HOpenCV_warp.h &release_mem_storage"
  cp_release_mem_storage :: FunPtr (Ptr CvMemStorage -> IO ())

createMemStorageF :: CInt -> IO (ForeignPtr CvMemStorage)
createMemStorageF = (createForeignPtr cp_release_mem_storage) . cvCreateMemStorage
  

-- images / matrices / arrays

foreign import ccall unsafe "HOpenCV_warp.h create_image"
  c_cvCreateImage :: CInt -> CInt -> CInt -> CInt -> IO (Ptr IplImage)

cvCreateImage :: CvSize -> CInt -> Depth -> IO (Ptr IplImage)
cvCreateImage size numChans depth = errorName "Failed to create image" . checkPtr $ c_cvCreateImage (sizeWidth size) (sizeHeight size) (unDepth depth) numChans

foreign import ccall unsafe "HOpenCV_warp.h release_image"
  cvReleaseImage :: Ptr IplImage -> IO ()

foreign import ccall unsafe "HOpenCV_warp.h &release_image"
  cp_release_image :: FunPtr (Ptr IplImage -> IO ())

createImageF :: CvSize -> CInt -> Depth -> IO (ForeignPtr IplImage)
createImageF x y z = createForeignPtr cp_release_image $ cvCreateImage x y z

foreign import ccall unsafe "cxcore.h cvCloneImage"
  c_cvCloneImage :: Ptr IplImage -> IO (Ptr IplImage)

cvCloneImage :: Ptr IplImage -> IO (Ptr IplImage)
cvCloneImage = errorName "Failed to clone image" . checkPtr . c_cvCloneImage
                  
cloneImageF :: Ptr IplImage -> IO (ForeignPtr IplImage)
cloneImageF x = createForeignPtr cp_release_image $ cvCloneImage x
  
foreign import ccall unsafe "HOpenCV_warp.h get_size"
  c_get_size :: Ptr CvArr -> Ptr CvSize -> IO ()

cvGetSize :: IplArrayType a => Ptr a -> CvSize
cvGetSize p = unsafePerformIO $
              alloca $ \cvSizePtr -> do
                c_get_size (castPtr p) cvSizePtr
                size <- peek cvSizePtr
                return size

foreign import ccall unsafe "HOpenCV_warp.h get_depth"
  c_get_depth :: Ptr IplImage -> IO CInt

getDepth :: Ptr IplImage -> IO Depth
getDepth img = do
  depthInt <- c_get_depth img
  case numToDepth depthInt of
    Nothing -> fail "Bad depth in image struct"
    Just depth -> return depth

foreign import ccall unsafe "HOpenCV_warp.h get_nChannels"
  c_get_nChannels :: Ptr IplImage -> IO CInt

getNumChannels :: Integral a => Ptr IplImage -> IO a
getNumChannels img = fmap fromIntegral $ c_get_nChannels img


foreign import ccall unsafe "cxcore.h cvConvertScale"
  cvConvertScale :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()

                                
foreign import ccall unsafe "HOpenCV_warp.h cv_free"
  cvFree :: Ptr a -> IO ()
            
foreign import ccall unsafe "cxcore.h cvLoad"
  c_cvLoad :: CString -> Ptr CvMemStorage -> CString -> Ptr CString -> IO (Ptr a)

cvLoad :: String -> Ptr CvMemStorage -> Maybe String -> IO (Ptr a, Maybe String)
cvLoad filename memstorage name = withCString filename cvLoad'
    where cvLoad' filenameC = do
            case name of
              Nothing -> cvLoad'' filenameC nullPtr
              Just n' -> withCString n' $ cvLoad'' filenameC
          cvLoad'' filenameC nameC = alloca $ \ptrRealNameC -> do
              ptrObj <- c_cvLoad filenameC memstorage nameC ptrRealNameC
              realNameC <- peek ptrRealNameC
              realName <- if realNameC == nullPtr 
                          then return Nothing 
                          else fmap Just $ peekCString realNameC
              cvFree realNameC
              return (ptrObj, realName)
              
foreign import ccall unsafe "cxcore.h cvGetSeqElem"
  cvGetSeqElem :: Ptr (CvSeq a) -> CInt -> IO (Ptr a)
  
foreign import ccall unsafe "HOpenCV_warp.h c_rect_cvGetSeqElem"
  cvGetSeqElemRect :: Ptr (CvSeq CvRect) -> CInt -> IO (Ptr CvRect)

foreign import ccall unsafe "HOpenCV_warp.h seq_total"
  seqNumElems :: Ptr (CvSeq a) -> IO CInt

seqToList :: Ptr (CvSeq a) -> IO [Ptr a]
seqToList pseq = do
  numElems <- seqNumElems pseq
  mapM (cvGetSeqElem pseq) [1..(numElems)]

seqToRectList :: Ptr (CvSeq CvRect) -> IO [CvRect]
seqToRectList pseq = do
  numElems <- seqNumElems pseq
  flip mapM [1..(numElems)] $ \i -> do
    rectP <- cvGetSeqElemRect pseq i
    rect <- peek rectP
    return rect

------------------------------------------------------------------------------
-- Debugging stuff, not part of opencv
foreign import ccall unsafe "HOpenCV_warp.h debug_print_image_header"
  c_debug_print_image_header :: Ptr IplImage -> IO ()
