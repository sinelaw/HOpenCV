{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeFamilies #-}

module AI.CV.OpenCV.CxCore where

import Foreign.ForeignPtrWrap
import Foreign.C.Types
import Foreign.C.String
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe

import Data.VectorSpace as VectorSpace

#include <cxcore.h>

------------------------------------------------------
toFromIntegral :: (RealFrac c, Integral b, Integral a, Num b1) => (b1 -> c) -> a -> b
toFromIntegral f = round . f . fromIntegral

toFromIntegral2 :: (Integral a, Num b, Integral a1, Num b1, RealFrac a2, Integral b2) => (b -> b1 -> a2) -> a -> a1 -> b2
toFromIntegral2 f x y = round (f (fromIntegral x) (fromIntegral y))
------------------------------------------------------

data CvSize  = CvSize { sizeWidth :: CInt, sizeHeight :: CInt }
               deriving (Show, Eq)
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

liftCvSize ::(RealFrac c, Num b) => (b -> c) -> CvSize -> CvSize
liftCvSize f (CvSize w h) = CvSize (f' w) (f' h)
    where f' = toFromIntegral f

liftCvSize2 :: (Num b, Num b1, RealFrac a) => (b -> b1 -> a) -> CvSize -> CvSize -> CvSize
liftCvSize2 f (CvSize w1 h1) (CvSize w2 h2) = CvSize (f' w1 w2) (f' h1 h2)
    where f' = toFromIntegral2 f

instance AdditiveGroup CvSize where
  zeroV = CvSize 0 0
  (^+^) = liftCvSize2 (+)
  negateV = liftCvSize (0-)

instance VectorSpace CvSize where
  type Scalar CvSize = Double -- todo: use CInt instead of Double here?
  a *^ s = liftCvSize (a*) s


data CvRect  = CvRect { rectX :: CInt, rectY :: CInt, rectWidth :: CInt, rectHeight :: CInt }
               deriving (Show, Eq)
                        
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
        

liftCvRect :: (RealFrac c, Num b) => (b -> c) -> CvRect -> CvRect
liftCvRect f (CvRect x y w h) = CvRect (f' x) (f' y) (f' w) (f' h)
    where f' = toFromIntegral f

liftCvRect2 :: (Num b, Num b1, RealFrac a) => (b -> b1 -> a) -> CvRect -> CvRect -> CvRect
liftCvRect2 f (CvRect x1 y1 w1 h1) (CvRect x2 y2 w2 h2) = CvRect (f' x1 x2) (f' y1 y2) (f' w1 w2) (f' h1 h2)
    where f' = toFromIntegral2 f

instance AdditiveGroup CvRect where
  zeroV = CvRect 0 0 0 0
  (^+^) = liftCvRect2 (+)
  negateV = liftCvRect (0-)

instance VectorSpace CvRect where
  type Scalar CvRect = Double -- todo: use CInt instead of Double here?
  a *^ r = liftCvRect (a*) r
  


------------------------------------------------------
class IplArrayType a
 where
  fromArr :: a -> CvArr

data Priv_CvArr
newtype CvArr = CvArr (Ptr Priv_CvArr)

instance IplArrayType CvArr
 where
  fromArr (CvArr p) = CvArr $ castPtr p

data Priv_IplImage
newtype IplImage  = IplImage  (Ptr        Priv_IplImage)
newtype IplImageF = IplImageF (ForeignPtr Priv_IplImage)

instance IplArrayType IplImage
 where
  fromArr (IplImage p) = CvArr $ castPtr p

data Priv_CvMemStorage
type CvMemStorage  = Ptr        Priv_CvMemStorage
type CvMemStorageF = ForeignPtr Priv_CvMemStorage

data Priv_CvSeq a
type CvSeq a = Ptr (Priv_CvSeq a)

-- fromArr :: IplArrayType a => Ptr a -> Ptr CvArr
-- fromArr = castPtr 

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
  c_cvCreateMemStorage :: CInt -> IO (Ptr Priv_CvMemStorage)

cvCreateMemStorage :: CInt -> IO CvMemStorage
cvCreateMemStorage = errorName "Failed to create mem storage" . checkPtr . c_cvCreateMemStorage 

foreign import ccall unsafe "HOpenCV_wrap.h release_mem_storage"
  cvReleaseMemStorage :: Ptr Priv_CvMemStorage -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h &release_mem_storage"
  cp_release_mem_storage :: FunPtr (Ptr Priv_CvMemStorage -> IO ())

createMemStorageF :: CInt -> IO CvMemStorageF
createMemStorageF = (createForeignPtr cp_release_mem_storage) . cvCreateMemStorage
  

-- images / matrices / arrays

foreign import ccall unsafe "HOpenCV_wrap.h create_image"
  c_cvCreateImage :: CInt -> CInt -> CInt -> CInt -> IO (Ptr Priv_IplImage)

cvCreateImage :: CvSize -> CInt -> Depth -> IO IplImage
cvCreateImage size numChans depth
  = do im <- errorName "Failed to create image" . checkPtr 
             $ c_cvCreateImage (sizeWidth size) (sizeHeight size) (unDepth depth) numChans
       return $ IplImage im

foreign import ccall unsafe "HOpenCV_wrap.h release_image"
  cvReleaseImage :: Ptr Priv_IplImage -> IO ()

releaseImage :: IplImage -> IO ()
releaseImage (IplImage p)
  = cvReleaseImage p

foreign import ccall unsafe "HOpenCV_wrap.h &release_image"
  cp_release_image :: FunPtr (Ptr Priv_IplImage -> IO ())


createImageF :: CvSize -> CInt -> Depth -> IO IplImageF
createImageF x y z
  = do IplImage p <- cvCreateImage x y z
       fp         <- createForeignPtr cp_release_image (return p)
       return $ IplImageF fp

foreign import ccall unsafe "cxcore.h cvCloneImage"
  c_cvCloneImage :: Ptr Priv_IplImage -> IO (Ptr Priv_IplImage)

cvCloneImage :: IplImage -> IO IplImage
cvCloneImage (IplImage p)
  = do p' <- errorName "Failed to clone image" . checkPtr . c_cvCloneImage $ p
       return $ IplImage p'
                  
cloneImageF :: IplImage -> IO IplImageF
cloneImageF iplIm
  = do (IplImage p) <- cvCloneImage iplIm
       fp           <- createForeignPtr cp_release_image (return p)
       return $ IplImageF fp
  
foreign import ccall unsafe "HOpenCV_wrap.h get_size"
  c_get_size :: Ptr Priv_CvArr -> Ptr CvSize -> IO ()

foreign import ccall unsafe "cxcore.h cvCopy"
  c_cvCopy :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> Ptr Priv_CvArr -> IO ()
                   
-- todo add mask support
cvCopy :: IplArrayType a => a -> a -> IO ()
cvCopy src dst
  = let (CvArr src' ) = fromArr src
        (CvArr dst') = fromArr dst
    in c_cvCopy src' dst' nullPtr

cvGetSize :: IplArrayType a => a -> CvSize
cvGetSize a = unsafePerformIO $
              alloca $ \cvSizePtr -> do
                let (CvArr p) = fromArr a
                c_get_size (castPtr p) cvSizePtr
                size <- peek cvSizePtr
                return size

foreign import ccall unsafe "HOpenCV_wrap.h get_depth"
  c_get_depth :: Ptr Priv_IplImage -> IO CInt

getDepth :: IplImage -> IO Depth
getDepth (IplImage img) = do
  depthInt <- c_get_depth img
  case numToDepth depthInt of
    Nothing -> fail "Bad depth in image struct"
    Just depth -> return depth

foreign import ccall unsafe "HOpenCV_wrap.h get_nChannels"
  c_get_nChannels :: Ptr Priv_IplImage -> IO CInt

getNumChannels :: Integral a => IplImage -> IO a
getNumChannels (IplImage img) = fmap fromIntegral $ c_get_nChannels img


foreign import ccall unsafe "cxcore.h cvConvertScale"
  cvConvertScale :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "HOpenCV_wrap.h cv_free"
  cvFree :: Ptr a -> IO ()
            
foreign import ccall unsafe "cxcore.h cvLoad"
  c_cvLoad :: CString -> Ptr Priv_CvMemStorage -> CString -> Ptr CString -> IO (Ptr a)

cvLoad :: String -> CvMemStorage -> Maybe String -> IO (Ptr a, Maybe String)
cvLoad filename mem name
  = withCString filename $ \filenameC ->
    case name
      of Nothing -> cvLoad'' filenameC nullPtr
         Just n' -> withCString n' $ cvLoad'' filenameC
 where
  cvLoad'' filenameC nameC
    = alloca $ \ptrRealNameC ->
        do ptrObj <- errorName "cvLoad failed" . checkPtr
                     $ c_cvLoad filenameC mem nameC ptrRealNameC
           realNameC <- peek ptrRealNameC
           realName <- if realNameC == nullPtr 
                         then return Nothing 
                         else fmap Just $ peekCString realNameC
           cvFree realNameC
           return (ptrObj, realName)
              
foreign import ccall unsafe "cxcore.h cvGetSeqElem"
  cvGetSeqElem :: Ptr (Priv_CvSeq a) -> CInt -> IO (Ptr a)
  
-- foreign import ccall unsafe "HOpenCV_wrap.h c_rect_cvGetSeqElem"
--   cvGetSeqElemRect :: Ptr (CvSeq (Ptr CvRect)) -> CInt -> IO (Ptr CvRect)

foreign import ccall unsafe "HOpenCV_wrap.h seq_total"
  seqNumElems :: Ptr (Priv_CvSeq a) -> IO CInt

seqToPList :: CvSeq a -> IO [Ptr a]
seqToPList pseq = do
  numElems <- seqNumElems pseq
  mapM (cvGetSeqElem pseq) [1..(numElems)]

seqToList :: Storable a => CvSeq a -> IO [a]
seqToList pseq = do
  numElems <- seqNumElems pseq
  flip mapM [1..(numElems)] $ \i -> do
    elemP <- cvGetSeqElem pseq i
    elem' <- peek elemP
    return elem'

-- seqToRectList :: Ptr (CvSeq (Ptr CvRect)) -> IO [CvRect]
-- seqToRectList pseq = do
--   numElems <- seqNumElems pseq
--   flip mapM [1..(numElems)] $ \i -> do
--     rectP <- cvGetSeqElemRect pseq i
--     rect <- peek rectP
--     return rect

foreign import ccall unsafe "HOpenCV_wrap.h c_cvRectangle"
  c_cvRectangle :: Ptr Priv_CvArr -> CInt -> CInt -> CInt -> CInt -> IO ()

cvRectangle :: IplArrayType a => a -> CvRect -> IO ()
cvRectangle dst (CvRect x y w h)
  = let CvArr dst' = fromArr dst
    in c_cvRectangle dst' x y w h

------------------------------------------------------------------------------
-- Debugging stuff, not part of opencv

-- | Debugging function to print some of the internal details of an IplImage structure
foreign import ccall unsafe "HOpenCV_wrap.h debug_print_image_header"
  c_debug_print_image_header :: Ptr Priv_IplImage -> IO ()
