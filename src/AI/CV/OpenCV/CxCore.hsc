{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeFamilies #-}

module AI.CV.OpenCV.CxCore where
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.ForeignPtrWrap
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Data.VectorSpace as VectorSpace

#include <opencv/cxcore.h>

------------------------------------------------------
toFromIntegral :: (RealFrac c, Integral b, Integral a, Num b1) => (b1 -> c) -> a -> b
toFromIntegral f = round . f . fromIntegral

toFromIntegral2 :: (Integral a, Num b, Integral a1, Num b1, RealFrac a2, Integral b2) => 
                   (b -> b1 -> a2) -> a -> a1 -> b2
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
validDepths = [iplDepth1u, iplDepth8u, iplDepth8s, iplDepth16u, 
               iplDepth16s, iplDepth32s, iplDepth32f, iplDepth64f]

depthsLookupList :: [(CInt, Depth)]
depthsLookupList = map (\d -> (unDepth d, d)) validDepths

numToDepth :: CInt -> Maybe Depth
numToDepth x = lookup x depthsLookupList
  

---------------------------------------------------------------
-- mem storage
foreign import ccall unsafe "opencv/cxcore.h cvCreateMemStorage"
  c_cvCreateMemStorage :: CInt -> IO (Ptr CvMemStorage)

cvCreateMemStorage :: CInt -> IO (Ptr CvMemStorage)
cvCreateMemStorage = errorName "Failed to create mem storage" . checkPtr . c_cvCreateMemStorage 

-- foreign import ccall unsafe "HOpenCV_wrap.h release_mem_storage"
--   cvReleaseMemStorage :: Ptr CvMemStorage -> IO ()

foreign import ccall unsafe "opencv/cxcore.h cvReleaseMemStorage"
  c_cvReleaseMemStorage :: Ptr (Ptr CvMemStorage) -> IO ()

cvReleaseMemStorage :: Ptr CvMemStorage -> IO ()
cvReleaseMemStorage mem = alloca $ \p -> poke p mem >> c_cvReleaseMemStorage p

foreign import ccall unsafe "HOpenCV_wrap.h &release_mem_storage"
  cp_release_mem_storage :: FunPtr (Ptr CvMemStorage -> IO ())

createMemStorageF :: CInt -> IO (ForeignPtr CvMemStorage)
createMemStorageF = (createForeignPtr cp_release_mem_storage) . cvCreateMemStorage

-- images / matrices / arrays

foreign import ccall unsafe "HOpenCV_wrap.h create_image"
  c_cvCreateImage :: CInt -> CInt -> CInt -> CInt -> IO (Ptr IplImage)

-- |Allocate memory for an 'IplImage' with the given dimensions,
-- number of color channels, and color depth.
cvCreateImage :: CvSize -> CInt -> Depth -> IO (Ptr IplImage)
cvCreateImage size numChans depth = 
    errorName "Failed to create image" . checkPtr $ 
    c_cvCreateImage (sizeWidth size) (sizeHeight size) (unDepth depth) numChans

-- foreign import ccall unsafe "HOpenCV_wrap.h release_image"
--   cvReleaseImage :: Ptr IplImage -> IO ()
foreign import ccall unsafe "opencv/cxcore.h cvReleaseImage"
  c_cvReleaseImage :: Ptr (Ptr IplImage) -> IO ()

-- |Release the memory allocated to an 'IplImage'.
cvReleaseImage :: Ptr IplImage -> IO ()
cvReleaseImage mem = alloca $ \p -> poke p mem >> c_cvReleaseImage p

foreign import ccall unsafe "HOpenCV_wrap.h &release_image"
  cp_release_image :: FunPtr (Ptr IplImage -> IO ())

createImageF :: CvSize -> CInt -> Depth -> IO (ForeignPtr IplImage)
createImageF x y z = createForeignPtr cp_release_image $ cvCreateImage x y z

foreign import ccall unsafe "opencv/cxcore.h cvCloneImage"
  c_cvCloneImage :: Ptr IplImage -> IO (Ptr IplImage)

cvCloneImage :: Ptr IplImage -> IO (Ptr IplImage)
cvCloneImage = errorName "Failed to clone image" . checkPtr . c_cvCloneImage
                  
cloneImageF :: Ptr IplImage -> IO (ForeignPtr IplImage)
cloneImageF x = createForeignPtr cp_release_image $ cvCloneImage x
  
foreign import ccall unsafe "HOpenCV_wrap.h get_size"
  c_get_size :: Ptr CvArr -> Ptr CvSize -> IO ()

foreign import ccall unsafe "opencv/cxcore.h cvCopy"
  c_cvCopy :: Ptr CvArr -> Ptr CvArr -> Ptr CvArr -> IO ()
                   
-- todo add mask support
cvCopy :: IplArrayType a => Ptr a -> Ptr a -> IO ()
cvCopy src dst = c_cvCopy (fromArr src) (fromArr dst) nullPtr

cvGetSize :: IplArrayType a => Ptr a -> CvSize
cvGetSize p = unsafePerformIO $
              alloca $ \cvSizePtr -> do
                c_get_size (castPtr p) cvSizePtr
                size <- peek cvSizePtr
                return size

foreign import ccall unsafe "HOpenCV_wrap.h get_depth"
  c_get_depth :: Ptr IplImage -> IO CInt

getDepth :: Ptr IplImage -> IO Depth
getDepth img = do
  depthInt <- c_get_depth img
  case numToDepth depthInt of
    Nothing -> fail "Bad depth in image struct"
    Just depth -> return depth

foreign import ccall unsafe "HOpenCV_wrap.h get_nChannels"
  c_get_nChannels :: Ptr IplImage -> IO CInt

getNumChannels :: Integral a => Ptr IplImage -> IO a
getNumChannels img = fmap fromIntegral $ c_get_nChannels img


foreign import ccall unsafe "opencv/cxcore.h cvConvertScale"
  cvConvertScale :: Ptr CvArr -> Ptr CvArr -> CDouble -> CDouble -> IO ()
                                
foreign import ccall unsafe "HOpenCV_wrap.h cv_free"
  cvFree :: Ptr a -> IO ()
            
foreign import ccall unsafe "opencv/cxcore.h cvLoad"
  c_cvLoad :: CString -> Ptr CvMemStorage -> CString -> Ptr CString -> IO (Ptr a)

cvLoad :: String -> Ptr CvMemStorage -> Maybe String -> IO (Ptr a, Maybe String)
cvLoad filename memstorage name = withCString filename cvLoad'
    where cvLoad' filenameC = do
            case name of
              Nothing -> cvLoad'' filenameC nullPtr
              Just n' -> withCString n' $ cvLoad'' filenameC
          cvLoad'' filenameC nameC = alloca $ \ptrRealNameC -> do
              ptrObj <- errorName "cvLoad failed" . checkPtr $ c_cvLoad filenameC memstorage nameC ptrRealNameC
              realNameC <- peek ptrRealNameC
              realName <- if realNameC == nullPtr 
                          then return Nothing 
                          else fmap Just $ peekCString realNameC
              cvFree realNameC
              return (ptrObj, realName)
              
foreign import ccall unsafe "opencv/cxcore.h cvGetSeqElem"
  cvGetSeqElem :: Ptr (CvSeq a) -> CInt -> IO (Ptr a)
  
-- foreign import ccall unsafe "HOpenCV_wrap.h c_rect_cvGetSeqElem"
--   cvGetSeqElemRect :: Ptr (CvSeq (Ptr CvRect)) -> CInt -> IO (Ptr CvRect)

foreign import ccall unsafe "HOpenCV_wrap.h seq_total"
  seqNumElems :: Ptr (CvSeq a) -> IO CInt

seqToPList :: Ptr (CvSeq a) -> IO [Ptr a]
seqToPList pseq = do
  numElems <- seqNumElems pseq
  mapM (cvGetSeqElem pseq) [1..(numElems)]

seqToList :: Storable a => Ptr (CvSeq a) -> IO [a]
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
  c_cvRectangle :: Ptr CvArr -> CInt -> CInt -> CInt -> CInt -> IO ()

cvRectangle :: IplArrayType a => Ptr a -> CvRect -> IO ()
cvRectangle dst (CvRect x y w h) = c_cvRectangle (fromArr dst) x y w h

foreign import ccall unsafe "HOpenCV_wrap.h c_cvLine"
        c_cvLine :: Ptr CvArr -> CInt -> CInt  -> CInt -> CInt -> 
                    CDouble -> CDouble -> CDouble -> CInt -> 
                    CInt -> CInt -> IO ()

cvLine :: IplArrayType a => Ptr a -> (Int, Int)  -> (Int, Int) -> 
                    (Double, Double, Double) -> Int -> 
                    Int -> IO ()
cvLine dst (x1,y1) (x2,y2) (r,g,b) thickness lineType =
    c_cvLine (fromArr dst) (fi x1) (fi y1) (fi x2) (fi y2) 
             (fr r) (fr g) (fr b) (fi thickness) (fi lineType) 0 
        where fi = fromIntegral
              fr = realToFrac

------------------------------------------------------------------------------
-- Debugging stuff, not part of opencv

-- | Debugging function to print some of the internal details of an IplImage structure
foreign import ccall unsafe "HOpenCV_wrap.h debug_print_image_header"
  c_debug_print_image_header :: Ptr IplImage -> IO ()
