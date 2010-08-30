{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables, 
             TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs #-}
module AI.CV.OpenCV.Core.HIplImage 
    ( FreshImage, TriChromatic, MonoChromatic, HasChannels(..), HasDepth(..), 
      HIplImage(..), mkHIplImage, mkBlackImage, withHIplImage, bytesPerPixel, 
      ByteOrFloat, HasScalar(..), IsCvScalar(..)) where
import AI.CV.OpenCV.Core.CxCore (IplImage,Depth(..),iplDepth8u, iplDepth16u,
                                 iplDepth32f, iplDepth64f)
import AI.CV.OpenCV.Core.CV (cvCvtColor)
import AI.CV.OpenCV.Core.ColorConversion (cv_GRAY2BGR, cv_BGR2GRAY)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (complement, (.&.))
import Data.Word (Word8, Word16)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable
import Unsafe.Coerce

#include <opencv/cxtypes.h>
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

-- |Type annotation indicating that an 'HIplImage' is freshly
-- allocated. This is used to drive the allocation fusion mechanism
-- that may perform in-place updates when an operation is composed
-- with a function that returns a fresh image.
data FreshImage

data TriChromatic
data MonoChromatic

class HasChannels a where
    numChannels :: a -> Int

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

class HasDepth a => ByteOrFloat a where
instance ByteOrFloat Word8 where
instance ByteOrFloat Float where

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

instance IsCvScalar Word8 where toCvScalar = depthToScalar
instance IsCvScalar Word16 where toCvScalar = depthToScalar
instance IsCvScalar Float where toCvScalar = depthToScalar
instance IsCvScalar Double where toCvScalar = depthToScalar
instance (HasDepth d, IsCvScalar d) => IsCvScalar (d,d,d) where
    toCvScalar (r,g,b) = let f = realToFrac . toDouble
                         in (f r, f g, f b, 0)

depthToScalar :: HasDepth d => d -> (CDouble, CDouble, CDouble, CDouble)
depthToScalar x = let x' = realToFrac (toDouble x)
                  in (x', x', x', x')

bytesPerPixel :: HasDepth d => d -> Int
bytesPerPixel = (`div` 8) . fromIntegral . unSign . unDepth . depth
    where unSign = (complement #{const IPL_DEPTH_SIGN} .&.)


-- |A data structure representing the information OpenCV uses from an
-- 'IplImage' struct. It includes the pixel origin, image width, image
-- height, image size (number of bytes), a pointer to the pixel data,
-- and the row stride. Its type is parameterized by whether or not the
-- backing pixel data is fresh (vs shared), the number of color
-- channels (i.e. 'MonoChromatic' or 'TriChromatic'), and the pixel
-- depth (e.g. 'Word8', 'Float').
data HIplImage a c d = (HasChannels c, HasDepth d) => 
                       HIplImage { origin    :: Int
                                 , width     :: Int
                                 , height    :: Int
                                 , imageSize :: Int
                                 , imageData :: ForeignPtr d
                                 , widthStep :: Int }

-- |Prepare a 'HIplImage' of the given width and height. The pixel and
-- color depths are gleaned from the type, and may often be inferred.
mkHIplImage :: forall c d. (HasChannels c, HasDepth d) => 
               Int -> Int -> IO (HIplImage FreshImage c d)
mkHIplImage w h = 
    do ptr <- mallocForeignPtrArray numBytes
       return $ HIplImage 0 w h numBytes ptr stride
    where numBytes = stride * h
          bpp = bytesPerPixel (undefined::d)
          stride = w * (numChannels (undefined::c) :: Int) * bpp

foreign import ccall unsafe "memset"
  memset :: Ptr Word8 -> Word8 -> CInt -> IO ()

-- |Prepare a 'HIplImage' of the given width and height. Set all
-- pixels to zero.
mkBlackImage :: (HasChannels c, HasDepth d, Integral a) => 
                a -> a -> IO (HIplImage FreshImage c d)
mkBlackImage w h = do img <- mkHIplImage (fromIntegral w) (fromIntegral h)
                      let sz = fromIntegral $ imageSize img
                      withForeignPtr (imageData img) $ \ptr ->
                          memset (castPtr ptr) 0 sz
                      return img

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'HIplImage'.
withHIplImage :: (HasChannels c, HasDepth d) =>
                 HIplImage a c d -> (Ptr IplImage -> IO b) -> IO b
withHIplImage img f = alloca $ 
                      \p -> withForeignPtr (imageData img) 
                                           (\hp -> pokeIpl img p (castPtr hp) >>
                                                   f (castPtr p))

-- Poke a 'Ptr' 'HIplImage' with a specific imageData 'Ptr' that is
-- currently valid. This is solely an auxiliary function to
-- 'withHIplImage'.
pokeIpl :: forall a c d. (HasChannels c, HasDepth d) => 
           HIplImage a c d -> Ptr (HIplImage a c d) -> Ptr Word8 -> IO ()
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
instance forall a c d. (HasChannels c, HasDepth d) => 
    Storable (HIplImage a c d) where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke = error "Poking a Ptr HIplImage is unsafe."
    peek ptr = do
      numChannels' <- (#peek IplImage, nChannels) ptr :: IO Int
      depth' <- Depth <$> (#peek IplImage, depth) ptr
      width' <- (#peek IplImage, width) ptr
      height' <- (#peek IplImage, height) ptr
      when (depth' /= (depth (undefined::d)))
           (error $ "IplImage has depth "++show depth'++
                    " but desired HIplImage has depth "++
                    show (depth (undefined::d)))
      if numChannels (undefined::c) /= numChannels'
        then do img2 <- mkHIplImage width' height' :: IO (HIplImage FreshImage c d)
                let conv = if numChannels' == 1 
                           then cv_GRAY2BGR
                           else cv_BGR2GRAY
                    ptr' = castPtr ptr :: Ptr IplImage
                withHIplImage img2 $ \dst -> cvCvtColor ptr' dst conv
                return $ unsafeCoerce img2
        else do origin' <- (#peek IplImage, origin) ptr
                imageSize' <- (#peek IplImage, imageSize) ptr
                imageData' <- (#peek IplImage, imageData) ptr >>= newForeignPtr_
                widthStep' <- (#peek IplImage, widthStep) ptr
                return $ HIplImage origin' width' height' imageSize' 
                                   imageData' widthStep'
