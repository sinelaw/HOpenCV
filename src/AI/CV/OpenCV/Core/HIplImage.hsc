{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables, 
             TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module AI.CV.OpenCV.Core.HIplImage 
    ( TriChromatic, MonoChromatic, HasChannels(..), HasDepth(..), 
      HIplImage(..), mkHIplImage, mkBlackImage, withHIplImage, bytesPerPixel, 
      ByteOrFloat, HasScalar(..), IsCvScalar(..), freeROI, c_cvSetImageROI, 
      c_cvResetImageROI, origin, width, height, imageSize, roi, imageData, 
      widthStep, imageDataOrigin, addROI, resetROI, ImgBuilder(..), 
      HasROI, NoROI) where
import AI.CV.OpenCV.Core.CxCore (IplImage,Depth(..),iplDepth8u, iplDepth16u,
                                 iplDepth32f, iplDepth64f, cvFree, CvRect(..))
import AI.CV.OpenCV.Core.CV (cvCvtColor)
import AI.CV.OpenCV.Core.ColorConversion (cv_GRAY2BGR, cv_BGR2GRAY)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (complement, (.&.))
import Data.Word (Word8, Word16)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce

#include <opencv2/core/types_c.h>
{-
typedef struct _IplImage
{
    int  nSize;
    int  ID;
    int  nChannels;
    int  alphaChannel;
    int  depth;
    char colorModel[4];
    char channelSeq[4];
    int  dataOrder;
    int  origin;
    int  align;
    int  width;
    int  height;
    struct _IplROI *roi;
    struct _IplImage *maskROI;
    void  *imageId;
    struct _IplTileInfo *tileInfo;
    int  imageSize;
    char *imageData;
    int  widthStep;
    int  BorderMode[4];
    int  BorderConst[4];
    char *imageDataOrigin;
}
IplImage;
-}

data TriChromatic
data MonoChromatic

class HasChannels a where
    numChannels :: a -> CInt

class (Storable a, Num a) => HasDepth a where
    depth      :: a -> Depth
    toDouble   :: a -> Double
    fromDouble :: Double -> a

instance HasChannels TriChromatic where numChannels _ = 3
instance HasChannels MonoChromatic where numChannels _ = 1
instance HasDepth Word8 where 
    depth _ = iplDepth8u
    toDouble = fromIntegral
    fromDouble = round
instance HasDepth Word16 where 
    depth _ = iplDepth16u
    toDouble = fromIntegral
    fromDouble = round
instance HasDepth Float where 
    depth _ = iplDepth32f
    toDouble = realToFrac
    fromDouble = realToFrac
instance HasDepth Double where 
    depth _ = iplDepth64f
    toDouble = id
    fromDouble = id

class (HasDepth a, Num a) => ByteOrFloat a where
instance ByteOrFloat Word8 where
instance ByteOrFloat Float where

-- FIXME: Perhaps it would be better to use a distinct type for the
-- scalar type of color images? I'm having some trouble getting this
-- type to fit in, though.
--data RGB d = RGB !d !d !d

-- |An image with a particular number of channels have an associated
-- scalar type built from the type of its pixels. This class lets us
-- ensure that a scalar value to be used in an operation with an image
-- is compatible with that image.
class HasDepth d => HasScalar c d where
    type CvScalar c d

instance HasDepth d => HasScalar MonoChromatic d where
    type CvScalar MonoChromatic d = d

instance HasDepth d => HasScalar TriChromatic d where
    type CvScalar TriChromatic d = (d,d,d)

class IsCvScalar x where
    toCvScalar :: x -> (CDouble, CDouble, CDouble, CDouble)
    fromCvScalar :: (CDouble, CDouble, CDouble, CDouble) -> x

instance IsCvScalar Word8 where
    toCvScalar = depthToScalar
    fromCvScalar (r,_,_,_) = floor r

instance IsCvScalar Word16 where
    toCvScalar = depthToScalar
    fromCvScalar (r,_,_,_) = floor r

instance IsCvScalar Float where
    toCvScalar = depthToScalar
    fromCvScalar (r,_,_,_) = realToFrac r

instance IsCvScalar Double where
    toCvScalar = depthToScalar
    fromCvScalar (r,_,_,_) = realToFrac r

instance (HasDepth d, IsCvScalar d) => IsCvScalar (d,d,d) where
    toCvScalar (r,g,b) = let f = realToFrac . toDouble
                         in (f r, f g, f b, 0)
    fromCvScalar (r,g,b,_) = let f = fromDouble . realToFrac 
                             in (f r, f g, f b)

depthToScalar :: HasDepth d => d -> (CDouble, CDouble, CDouble, CDouble)
depthToScalar x = let x' = realToFrac (toDouble x)
                  in (x', x', x', x')

bytesPerPixel :: HasDepth d => d -> Int
bytesPerPixel = (`div` 8) . fromIntegral . unSign . unDepth . depth
    where unSign = (complement #{const IPL_DEPTH_SIGN} .&.)

-- |A data structure representing the information OpenCV uses from an
-- 'IplImage' struct. It includes the pixel origin, image width, image
-- height, image size (number of bytes), a pointer to the pixel data,
-- and the row stride. Its type is parameterized by the number of
-- color channels (i.e. 'MonoChromatic' or 'TriChromatic'), and the
-- pixel depth (e.g. 'Word8', 'Float').
-- data HIplImage c d = (HasChannels c, HasDepth d) => 
--                      HIplImage { origin          :: {-# UNPACK #-} !CInt
--                                , width           :: {-# UNPACK #-} !CInt
--                                , height          :: {-# UNPACK #-} !CInt
--                                , roi             :: !(Maybe CvRect)
--                                , imageSize       :: {-# UNPACK #-} !CInt
--                                , imageData       :: {-# UNPACK #-} !(ForeignPtr d)
--                                , imageDataOrigin :: {-# UNPACK #-} !(ForeignPtr d)
--                                , widthStep       :: {-# UNPACK #-} !CInt }

data HasROI
data NoROI

data HIplImage c d r where
  Img :: (HasChannels c, HasDepth d) => 
         !CInt -> !CInt -> !CInt -> !CInt -> !(ForeignPtr d) -> !(ForeignPtr d) -> 
         !CInt -> HIplImage c d NoROI
  ImgR :: (HasChannels c, HasDepth d) => 
          !CInt -> !CInt -> !CInt -> !CvRect -> !CInt -> !(ForeignPtr d) -> 
          !(ForeignPtr d) -> !CInt -> HIplImage c d HasROI

origin :: HIplImage c d r -> CInt
origin (Img o _ _ _ _ _ _) = o
origin (ImgR o _ _ _ _ _ _ _) = o

imageSize :: HIplImage c d r -> CInt
imageSize (Img _ _ _ s _ _ _) = s
imageSize (ImgR _ _ _ _ s _ _ _) = s

roi :: HIplImage c d r -> Maybe CvRect
roi (ImgR _ _ _ r _ _ _ _) = Just r
roi _ = Nothing

imageData :: HIplImage c d r -> ForeignPtr d
imageData (Img _ _ _ _ p _ _) = p
imageData (ImgR _ _ _ _ _ p _ _) = p

imageDataOrigin :: HIplImage c d r -> ForeignPtr d
imageDataOrigin (Img _ _ _ _ _ p _) = p
imageDataOrigin (ImgR _ _ _ _ _ _ p _) = p

width,height,widthStep :: HIplImage c d r -> CInt
width (Img _ w _ _ _ _ _) = w
width (ImgR _ w _ _ _ _ _ _) = w
height (Img _ _ h _ _ _ _) = h
height (ImgR _ _ h _ _ _ _ _) = h
widthStep (Img _ _ _ _ _ _ ws) = ws
widthStep (ImgR _ _ _ _ _ _ _ ws) = ws

addROI :: CvRect -> HIplImage c d r -> HIplImage c d HasROI
addROI r (Img o w h sz d ido ws) = ImgR o w h r sz d ido ws
addROI r (ImgR o w h _ sz d ido ws) = ImgR o w h r sz d ido ws
{-# INLINE addROI #-}

resetROI :: HIplImage c d r -> HIplImage c d NoROI
resetROI x@(Img _ _ _ _ _ _ _) = x
resetROI (ImgR o w h _ sz d ido ws) = Img o w h sz d ido ws
{-# INLINE resetROI #-}

-- |Prepare a 'HIplImage' of the given width and height. The pixel and
-- color depths are gleaned from the type, and may often be inferred.
mkHIplImage :: forall a c d. (HasChannels c, HasDepth d, Integral a) => 
               a -> a -> IO (HIplImage c d NoROI)
mkHIplImage w' h' = 
    do ptr <- mallocForeignPtrArray (fromIntegral numBytes)
       return $ Img 0 w h numBytes ptr ptr stride
    where w = fromIntegral w'
          h = fromIntegral h'
          numBytes = stride * h
          bpp = fi $ bytesPerPixel (undefined::d)
          stride = w * numChannels (undefined::c) * bpp
          fi = fromIntegral

foreign import ccall "memset"
  memset :: Ptr Word8 -> Word8 -> CInt -> IO ()

-- |Prepare a 'HIplImage' of the given width and height. Set all
-- pixels to zero.
mkBlackImage :: (HasChannels c, HasDepth d, Integral a) => 
                a -> a -> IO (HIplImage c d NoROI)
mkBlackImage w h = do img <- mkHIplImage (fromIntegral w) (fromIntegral h)
                      let sz = fromIntegral $ imageSize img
                      withForeignPtr (imageData img) $ \ptr ->
                          memset (castPtr ptr) 0 sz
                      return img

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'HIplImage'.
withHIplImage :: (HasChannels c, HasDepth d) =>
                 HIplImage c d r -> (Ptr IplImage -> IO b) -> IO b
withHIplImage img f = alloca $ 
                      \p -> withForeignPtr (imageData img) 
                                           (\hp -> pokeIpl img p (castPtr hp) >>
                                                   withROI img p f)

withROI :: (HasChannels c, HasDepth d) => 
           HIplImage c d r -> Ptr IplImage -> (Ptr IplImage -> IO a) -> IO a
withROI img p f = case roi img of
                    Nothing -> f p
                    Just (CvRect x y w h) -> do c_cvSetImageROI p x y w h
                                                r <- f p
                                                c_cvResetImageROI p
                                                return r

foreign import ccall "HOpenCV_wrap.h c_cvSetRoi"
  c_cvSetImageROI :: Ptr IplImage -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "opencv2/core/core_c.h cvResetImageROI"
  c_cvResetImageROI :: Ptr IplImage -> IO ()

foreign import ccall "HOpenCV_wrap.h c_cvGetROI"
  c_cvGetImageROI :: Ptr IplImage -> Ptr CInt -> IO ()

-- Poke a 'Ptr' 'IplImage' with a specific imageData 'Ptr' that is
-- currently valid. This is solely an auxiliary function to
-- 'withHIplImage'.
pokeIpl :: forall c d r. (HasChannels c, HasDepth d) => 
           HIplImage c d r -> Ptr IplImage -> Ptr Word8 -> IO ()
pokeIpl himg ptr hp =
    do (#poke IplImage, nSize) ptr ((#size IplImage)::Int)
       (#poke IplImage, ID) ptr (0::Int)
       (#poke IplImage, nChannels) ptr (numChannels (undefined::c))
       (#poke IplImage, depth) ptr (unDepth (depth (undefined::d)))
       (#poke IplImage, dataOrder) ptr (0::Int)
       (#poke IplImage, origin) ptr (origin himg)
       (#poke IplImage, align) ptr (4::Int)
       (#poke IplImage, width) ptr (width himg)
       (#poke IplImage, height) ptr (height himg)
       (#poke IplImage, roi) ptr nullPtr
       (#poke IplImage, maskROI) ptr nullPtr
       (#poke IplImage, imageId) ptr nullPtr
       (#poke IplImage, tileInfo) ptr nullPtr
       (#poke IplImage, imageSize) ptr (imageSize himg)
       (#poke IplImage, imageData) ptr hp
       (#poke IplImage, widthStep) ptr (widthStep himg)
       (#poke IplImage, imageDataOrigin) ptr hp

freeROI :: Ptr IplImage -> IO ()
freeROI ptr = do p <- (#peek IplImage, roi) ptr
                 if (ptrToIntPtr p == 0) then return () else cvFree p

maybePeek :: Ptr IplImage -> Ptr () -> IO (Maybe CvRect)
maybePeek img p | p == nullPtr = return Nothing
                | otherwise = allocaArray 4 $ 
                              \r -> do c_cvGetImageROI img r
                                       [x,y,w,h] <- peekArray 4 r
                                       return . Just $ CvRect x y w h

class ImgBuilder a where
  buildImg :: (HasChannels c, HasDepth d) =>
              CInt -> CInt -> CInt -> Maybe CvRect -> CInt ->
              ForeignPtr d -> ForeignPtr d -> CInt -> HIplImage c d a
  addMaybeROI :: Maybe CvRect -> (HIplImage c d r) -> HIplImage c d a

instance ImgBuilder NoROI where
  buildImg o w h Nothing sz d ido ws = Img o w h sz d ido ws
  buildImg _ _ _ _ _ _ _ _ = error "Building a NoROI image, but was given a ROI!"
  addMaybeROI Nothing x = resetROI x
  addMaybeROI _ _ = error "addMaybeROI tried to add a ROI to a NoROI Image!"

instance ImgBuilder HasROI where
  buildImg o w h (Just r) sz d ido ws = ImgR o w h r sz d ido ws
  buildImg _ _ _ _ _ _ _ _ = error "Building a ROI image, but wasn't given a ROI!"
  addMaybeROI (Just r) x = addROI r x
  addMaybeROI _ _ = error "addMaybeROI tried to add a null ROI to a HasROI Image!"

-- |An 'HIplImage' in Haskell is isomorphic with OpenCV's 'IplImage'
-- structure type. They share the same binary representation through
-- 'HIplImage' \'s 'Storable' instance. This allows for safe casts
-- between pointers of the two types. Note that obtaining an
-- 'HIplImage' from an 'IplImage' via 'peek' will not install a
-- Haskell finalizer on the underlying pixel data. That data is the
-- responsibility of the provider of the 'IplImage'. 'HIplImage'
-- values constructed within the Haskell runtime, on the other hand,
-- do have their underlying pixel data buffers registered with a
-- finalizer.
instance forall c d r. (HasChannels c, HasDepth d, ImgBuilder r) => 
    Storable (HIplImage c d r) where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke = error "Poking a Ptr HIplImage is unsafe."
    peek ptr = do
      numChannels' <- (#peek IplImage, nChannels) ptr :: IO CInt
      depth' <- Depth <$> (#peek IplImage, depth) ptr
      width' <- (#peek IplImage, width) ptr
      height' <- (#peek IplImage, height) ptr
      roir <- (#peek IplImage, roi) ptr >>= maybePeek (castPtr ptr)
      when (depth' /= (depth (undefined::d)))
           (error $ "IplImage has depth "++show depth'++
                    " but desired HIplImage has depth "++
                    show (depth (undefined::d)))
      if numChannels (undefined::c) /= numChannels'
        then do img2' <- mkHIplImage width' height' :: IO (HIplImage c d NoROI)
                let img2 = addMaybeROI roir img2' :: HIplImage c d r
                let conv = if numChannels' == 1 
                           then cv_GRAY2BGR
                           else cv_BGR2GRAY
                    ptr' = castPtr ptr :: Ptr IplImage
                withHIplImage img2 $ \dst -> cvCvtColor (castPtr ptr') 
                                                        (castPtr dst) 
                                                        conv
                (#peek IplImage, imageDataOrigin) ptr >>= cvFree
                return $ unsafeCoerce img2
        else do origin' <- (#peek IplImage, origin) ptr
                imageSize' <- (#peek IplImage, imageSize) ptr
                imageData' <- (#peek IplImage, imageData) ptr >>= newForeignPtr_
                imageDataOrigin' <- (#peek IplImage, imageDataOrigin) ptr >>= newForeignPtr_
                widthStep' <- (#peek IplImage, widthStep) ptr
                return $ buildImg origin' width' height' roir imageSize'
                                  imageData' imageDataOrigin' widthStep'
                -- return $ case roir of
                --            Nothing -> Img origin' width' height' imageSize' 
                --                           imageData' imageDataOrigin' widthStep'
                --            Just r -> ImgR origin' width' height' r imageSize' 
                --                           imageData' imageDataOrigin' widthStep'
