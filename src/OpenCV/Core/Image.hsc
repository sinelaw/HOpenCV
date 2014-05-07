{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables, 
             TypeFamilies, MultiParamTypeClasses, FlexibleInstances, GADTs, 
             BangPatterns, FlexibleContexts, TypeSynonymInstances,
             DataKinds, TemplateHaskell, ConstraintKinds #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-unused-binds #-}
module OpenCV.Core.Image (
    -- * Phantom types that statically describe image properties
    Channels(..), ROIEnabled(..),

    -- * Value-level reification of type-level properties
    HasDepth(..), 

    -- * Typed support for image operations that take scalar (color) parameters
    CvScalarT, AsCvScalar(..), ScalarOK,

    -- * Low-level image data structure
    Image(..), mkImage, mallocImage, blackImage, blackoutPixels, 
    withIplImage, bytesPerPixel, numChannels, peekIpl, pokeIpl,
    freeROI, c_cvSetImageROI, c_cvResetImageROI, setROI, resetROI, imageHasROI,
    UpdateROI(..)
  ) where
import OpenCV.Core.CxCore (IplImage,Depth(..),iplDepth8u, iplDepth16u, 
                           iplDepth16s, iplDepth32f, iplDepth64f, cvFree, 
                           CvRect(..), CvScalar(..))
import OpenCV.Core.CV (cvCvtColor)
import OpenCV.Core.ColorConversion (cv_GRAY2BGR, cv_BGR2GRAY)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Bits (complement, (.&.))
import Data.Int
import Data.Proxy
import Data.Singletons hiding (Proxy)
import Data.Singletons.TH
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

-- *Phantom types that statically describe image properties
data Channels = Trichromatic | Monochromatic
data ROIEnabled = HasROI | NoROI

genSingletons [''Channels, ''ROIEnabled]

-- NOTE: The singletons library defines various things for us that we
-- don't explicitly pattern match on or export, leading GHC to issue
-- unused binding warnings. We disable those warnings for this file.

numChannels' :: Channels -> Int
numChannels' Trichromatic = 3
numChannels' Monochromatic = 1

-- |Extract number of channels from the singleton value associated
-- with a type of the 'Channels' kind.
numChannels :: forall c. SingI c => Proxy (c::Channels) -> Int
numChannels _ = numChannels' . fromSing $ (sing::Sing c)

hasROI :: forall r. SingI r => Proxy (r::ROIEnabled) -> Bool
hasROI _ = case fromSing (sing::Sing r) of
             HasROI -> True
             _ -> False

imageHasROI :: forall c d r. SingI r => Image c d r -> Bool
imageHasROI _ = case fromSing (sing::Sing r) of
                  HasROI -> True
                  _ -> False

class (Storable a, Num a) => HasDepth a where
    depth      :: a -> Depth
    toDouble   :: a -> Double
    fromDouble :: Double -> a

instance HasDepth Word8 where 
    depth _ = iplDepth8u
    toDouble = fromIntegral
    fromDouble = round
instance HasDepth Word16 where 
    depth _ = iplDepth16u
    toDouble = fromIntegral
    fromDouble = round
instance HasDepth Int16 where 
    depth _ = iplDepth16s
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

-- |An image with a particular number of channels have an associated
-- scalar type built from the type of its pixels. This class lets us
-- ensure that a scalar value to be used in an operation with an image
-- is compatible with that image.
type family CvScalarT (c::Channels) d :: *
type instance CvScalarT Monochromatic d = d
type instance CvScalarT Trichromatic d = (d,d,d)

type ScalarOK s c d = (AsCvScalar s, s ~ CvScalarT c d)

-- |Scalar types are often round-tripped via doubles in OpenCV to
-- allow for non-overloaded interfaces of functions with scalar
-- parameters.
class AsCvScalar x where
    toCvScalar :: x -> CvScalar
    fromCvScalar :: CvScalar -> x

instance AsCvScalar Word8 where
    toCvScalar = depthToScalar
    fromCvScalar (CvScalar r _ _ _) = floor r

instance AsCvScalar Word16 where
    toCvScalar = depthToScalar
    fromCvScalar (CvScalar r _ _ _) = floor r

instance AsCvScalar Int16 where
    toCvScalar = depthToScalar
    fromCvScalar (CvScalar r _ _ _) = floor r

instance AsCvScalar Float where
    toCvScalar = depthToScalar
    fromCvScalar (CvScalar r _ _ _) = realToFrac r

instance AsCvScalar Double where
    toCvScalar = depthToScalar
    fromCvScalar (CvScalar r _ _ _) = realToFrac r

instance (HasDepth d, AsCvScalar d) => AsCvScalar (d,d,d) where
    toCvScalar (r,g,b) = let f = realToFrac . toDouble
                         in CvScalar (f r) (f g) (f b) 0
    fromCvScalar (CvScalar r g b _) = let f = fromDouble . realToFrac 
                                      in (f r, f g, f b)

depthToScalar :: HasDepth d => d -> CvScalar
depthToScalar x = let x' = realToFrac (toDouble x)
                  in CvScalar x' x' x' x'

bytesPerPixel :: HasDepth d => d -> Int
bytesPerPixel = (`div` 8) . fromIntegral . unSign . unDepth . depth
    where unSign = (complement #{const IPL_DEPTH_SIGN} .&.)

-- |A data structure representing the information OpenCV uses from an
-- 'IplImage' struct. It includes the pixel origin, image width, image
-- height, image size (number of bytes), a pointer to the pixel data,
-- and the row stride. Its type is parameterized by the number of
-- color channels (i.e. 'Monochromatic' or 'Trichromatic'), the pixel
-- depth (e.g. 'Word8', 'Float'), and whether or not the image has a
-- region-of-interest (ROI) set ('HasROI' or 'NoROI').
data Image (c::Channels) d (r::ROIEnabled) where
  Image :: (SingI c, HasDepth d, SingI r, UpdateROI r) => 
           { origin          :: !Int
           , width           :: !Int
           , height          :: !Int
           , roi             :: !(Maybe CvRect)
           , imageSize       :: !Int
           , imageData       :: !(ForeignPtr d)
           , imageDataOrigin :: !(ForeignPtr d)
           , widthStep       :: !Int
           } -> Image c d r


-- |@mkImage w h ptr@ makes an 'Image' of width, @w@, and height, @h@,
-- using pixel data at @ptr@. Pixels are assumed to be continuous, and
-- starting at the given pointer.
mkImage :: forall a c d. (Integral a, SingI c, HasDepth d) => 
           a -> a -> ForeignPtr d -> Image c d NoROI
mkImage w h pixels = Image 0 (fromIntegral w) (fromIntegral h) 
                           Nothing (fromIntegral h*stride) pixels pixels stride
  where stride = fromIntegral $
                 fromIntegral w * numChannels (Proxy::Proxy c) * bytesPerPixel (undefined::d)

-- |Set an image's region-of-interest.
setROI :: CvRect -> Image c d r -> Image c d HasROI
setROI r (Image o w h _ sz d ido ws) = Image o w h (Just r) sz d ido ws
{-# INLINE setROI #-}

-- |Clear any region-of-interest set for an image.
resetROI :: forall c d r. Image c d r -> Image c d NoROI
resetROI x@(Image o w h _ sz d ido ws)
  | hasROI (Proxy::Proxy r) = Image o w h Nothing sz d ido ws
  | otherwise = unsafeCoerce x
{-# INLINE resetROI #-}

-- |Prepare an 'Image' of the given width and height. The pixel and
-- color depths are gleaned from the type, and may often be inferred.
mallocImage :: forall a c d. (SingI c, HasDepth d, Integral a) => 
               a -> a -> IO (Image c d NoROI)
mallocImage w h = mkImage w h <$> mallocForeignPtrArray (fromIntegral numBytes)
  where numBytes = fromIntegral (w * h) * 
                   numChannels (Proxy::Proxy c) * bytesPerPixel (undefined::d)

foreign import ccall "memset"
  memset :: Ptr Word8 -> Word8 -> CInt -> IO ()

-- |Set an all of an 'Image'\'s pixels to black.
blackoutPixels :: Image c d r -> IO (Image c d r)
blackoutPixels img = do withForeignPtr (imageData img) $ \ptr ->
                          memset (castPtr ptr) 0 (fromIntegral $ imageSize img)
                        return img

-- |Prepare an 'Image' of the given width and height with all pixels
-- set to zero.
blackImage :: (SingI c, HasDepth d, Integral a) => 
              a -> a -> IO (Image c d NoROI)
blackImage w h = mallocImage w h >>= blackoutPixels

foreign import ccall "HOpenCV_wrap.h c_cvSetRoi"
  c_cvSetImageROI :: Ptr IplImage -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "opencv2/core/core_c.h cvResetImageROI"
  c_cvResetImageROI :: Ptr IplImage -> IO ()

withROI :: Image c d r -> Ptr IplImage -> (Ptr IplImage -> IO a) -> IO a
withROI img p f = case roi img of
                    Nothing -> f p
                    Just (CvRect x y w h) -> do c_cvSetImageROI p x y w h
                                                r <- f p
                                                c_cvResetImageROI p
                                                return r

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'Image'.
withIplImage :: Image c d r -> (Ptr IplImage -> IO b) -> IO b
withIplImage img@(Image{}) f = alloca $ \p ->
                               withForeignPtr (imageData img) 
                                              (\hp -> do pokeIpl img p (castPtr hp)
                                                         withROI img p f)

h2c :: Int -> CInt
h2c = fromIntegral

c2h :: CInt -> Int
c2h = fromIntegral

-- |Read a 'Image' from a 'Ptr' 'IplImage'
peekIpl :: (SingI c, HasDepth d, SingI r, UpdateROI r) => 
           Ptr IplImage -> IO (Image c d r)
peekIpl = peek . castPtr

-- Poke a 'Ptr' 'IplImage' with a specific imageData 'Ptr' that is
-- currently valid. This is solely an auxiliary function to
-- 'withHIplImage'.
pokeIpl :: forall c d r. (SingI c, HasDepth d) => 
           Image (c::Channels) d r -> Ptr IplImage -> Ptr Word8 -> IO ()
pokeIpl himg ptr hp =
    do (#poke IplImage, nSize) ptr ((#size IplImage)::CInt)
       (#poke IplImage, ID) ptr (0::CInt)
       (#poke IplImage, nChannels) ptr (h2c $ numChannels (Proxy::Proxy c))
       (#poke IplImage, depth) ptr (unDepth (depth (undefined::d)))
       (#poke IplImage, dataOrder) ptr (0::CInt)
       (#poke IplImage, origin) ptr (h2c $ origin himg)
       (#poke IplImage, align) ptr (4::CInt)
       (#poke IplImage, width) ptr (h2c $ width himg)
       (#poke IplImage, height) ptr (h2c $ height himg)
       (#poke IplImage, roi) ptr nullPtr
       (#poke IplImage, maskROI) ptr nullPtr
       (#poke IplImage, imageId) ptr nullPtr
       (#poke IplImage, tileInfo) ptr nullPtr
       (#poke IplImage, imageSize) ptr (h2c $ imageSize himg)
       (#poke IplImage, imageData) ptr hp
       (#poke IplImage, widthStep) ptr (h2c $ widthStep himg)
       (#poke IplImage, imageDataOrigin) ptr hp

foreign import ccall "HOpenCV_wrap.h c_cvGetROI"
  c_cvGetImageROI :: Ptr IplImage -> Ptr CInt -> IO ()

freeROI :: Ptr IplImage -> IO ()
freeROI ptr = do p <- (#peek IplImage, roi) ptr
                 if (ptrToIntPtr p == 0) then return () else cvFree p

maybePeekROI :: Ptr IplImage -> Ptr () -> IO (Maybe CvRect)
maybePeekROI img p | p == nullPtr = return Nothing
                   | otherwise = allocaArray 4 $ 
                                 \r -> do c_cvGetImageROI img r
                                          [x,y,w,h] <- peekArray 4 r
                                          return . Just $ CvRect x y w h

-- |An internal class that makes runtime guarantees about type level
-- ROI assertions.
class SingI a => UpdateROI (a::ROIEnabled) where
  updateROI :: Maybe CvRect -> Image c d a -> Image c d b

-- These functions are runtime checks that type-level guarantees are
-- met.
instance UpdateROI NoROI where
  updateROI Nothing x = unsafeCoerce $ resetROI x
  updateROI _ _ = error "Tried to update the ROI of a NoROI Image"

instance UpdateROI HasROI where
  updateROI (Just r) x = unsafeCoerce $ setROI r x
  updateROI _ _ = error "Tried to null out the ROI of a HasROI Image"

-- |An 'Image' in Haskell conforms closely to OpenCV's 'IplImage'
-- structure type. Note that obtaining an 'Image' from an 'IplImage'
-- via 'peek' will not install a Haskell finalizer on the underlying
-- pixel data. That data is the responsibility of the provider of the
-- 'IplImage'. 'Image' values constructed within the Haskell runtime,
-- on the other hand, will have their underlying pixel data buffers
-- managed by the garbage collector.
instance forall c d r. (SingI c, HasDepth d, SingI r, UpdateROI r) => 
         Storable (Image c d r) where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke = error "Poking a 'Ptr Image' is unsafe."
    peek ptr = do
      numChannels' <- c2h <$> (#peek IplImage, nChannels) ptr
      depth' <- Depth <$> (#peek IplImage, depth) ptr
      width' <- c2h <$> (#peek IplImage, width) ptr
      height' <- c2h <$> (#peek IplImage, height) ptr
      roir <- (#peek IplImage, roi) ptr >>= maybePeekROI (castPtr ptr)
      when (depth' /= (depth (undefined::d)))
           (error $ "IplImage has depth "++show depth'++
                    " but desired Image has depth "++
                    show (depth (undefined::d)))
      if numChannels (Proxy::Proxy c) /= numChannels'
        then do img2' <- mallocImage width' height' :: IO (Image c d NoROI)
                let img2 = updateROI roir img2' :: Image c d r
                let conv = if numChannels' == 1 
                           then cv_GRAY2BGR
                           else cv_BGR2GRAY
                    ptr' = castPtr ptr :: Ptr IplImage
                withIplImage img2 $ \dst -> cvCvtColor (castPtr ptr') 
                                                       (castPtr dst) 
                                                       conv
                (#peek IplImage, imageDataOrigin) ptr >>= cvFree
                return $ unsafeCoerce img2
        else do origin' <- c2h <$> (#peek IplImage, origin) ptr
                imageSize' <- c2h <$> (#peek IplImage, imageSize) ptr
                imageData' <- (#peek IplImage, imageData) ptr >>= newForeignPtr_
                imageDataOrigin' <- (#peek IplImage, imageDataOrigin) ptr >>= newForeignPtr_
                widthStep' <- c2h <$> (#peek IplImage, widthStep) ptr
                return $ Image origin' width' height' roir imageSize'
                               imageData' imageDataOrigin' widthStep'
