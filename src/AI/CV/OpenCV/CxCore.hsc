{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module AI.CV.OpenCV.CxCore where

import Foreign.C.Types
import Foreign

import Data.Maybe(fromJust)
import qualified Data.Map as Map

#include <cxcore.h>

------------------------------------------------------

data CvSize  = CvSize { width :: CInt, height :: CInt }

instance Storable CvSize where
    sizeOf    _ = (#size CvSize)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        w <- (#peek CvSize, width) ptr
        h <- (#peek CvSize, height) ptr
        return  CvSize { width = w, height = h }
    poke ptr (CvSize w h) = do
        (#poke CvSize, width) ptr w
        (#poke CvSize, height) ptr h

------------------------------------------------------
class IplArrayType a

data CvArr
instance IplArrayType CvArr

data IplImage
instance IplArrayType IplImage



data Depth = IPL_DEPTH_1U
             | IPL_DEPTH_8U -- Unsigned 8-bit integer
             | IPL_DEPTH_8S -- Signed 8-bit integer
             | IPL_DEPTH_16U -- Unsigned 16-bit integer
             | IPL_DEPTH_16S -- Signed 16-bit integer
             | IPL_DEPTH_32S -- Signed 32-bit integer
             | IPL_DEPTH_32F -- Single-precision floating point
             | IPL_DEPTH_64F -- Double-precision floating point
             deriving (Eq, Ord) -- for the map lookups
               
iplDepthSign :: CInt
iplDepthSign = 0x80000000

toIplDepthSigned :: CInt -> CInt
toIplDepthSigned x = x .|. iplDepthSign

depthNums :: [(Depth, CInt)]
depthNums = [(IPL_DEPTH_1U,  1),
             (IPL_DEPTH_8U,  8),
             (IPL_DEPTH_8S,  toIplDepthSigned 8),
             (IPL_DEPTH_16U, 16),
             (IPL_DEPTH_16S, toIplDepthSigned 16),
             (IPL_DEPTH_32S, toIplDepthSigned 32),
             (IPL_DEPTH_32F, 32),
             (IPL_DEPTH_64F, 64)]
            
mapDepthNums :: Map.Map Depth CInt
mapDepthNums = Map.fromList depthNums

mapNumDepths:: Map.Map CInt Depth
mapNumDepths = Map.fromList . map (\(x,y)->(y,x)) $ depthNums

depthToNum :: Depth -> CInt
depthToNum = fromJust . (flip Map.lookup $ mapDepthNums)

numToDepth :: CInt -> Maybe Depth
numToDepth = flip Map.lookup $ mapNumDepths

---------------------------------------------------------------

foreign import ccall unsafe "HOpenCV_warp.h create_image"
  c_cvCreateImage :: CInt -> CInt -> CInt -> CInt -> IO (Ptr IplImage)

cvCreateImage :: Integral a => CvSize -> a -> Depth -> IO (Ptr IplImage)
cvCreateImage size numChans depth = c_cvCreateImage (width size) (height size) (fromIntegral numChans) (depthToNum depth)

foreign import ccall unsafe "cxcore.h cvCloneImage"
  cvCloneImage :: Ptr IplImage -> IO (Ptr IplImage)
                  
foreign import ccall unsafe "HOpenCV_warp.h get_size"
  c_get_size :: Ptr CvArr -> Ptr CvSize -> IO ()

cvGetSize :: IplArrayType a => Ptr a -> CvSize
cvGetSize p = unsafePerformIO $
              alloca $ \cvSizePtr -> do
                c_get_size (castPtr p) cvSizePtr
                size <- peek cvSizePtr
                return size
