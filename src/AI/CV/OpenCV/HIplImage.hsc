{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, ScopedTypeVariables, GADTs #-}
module AI.CV.OpenCV.HIplImage 
    ( HIplImage, FreshImage, TriChromatic, MonoChromatic, HasChannels,
      HasDepth, width, height, imageSize, widthStep, pixels, pixelsCopy, 
      fromPtr, fromFileColor, fromFileGray, toFile, fromGrayPixels, isColor,
      fromColorPixels, withHIplImage, fromPixels, fromPixelsCopy,
      imgChannels, withCompatibleImage, withDuplicateImage, mkHIplImage, isMono)
    where
import AI.CV.OpenCV.CxCore (IplImage,Depth(..),iplDepth8u, iplDepth16u)
import AI.CV.OpenCV.CV (cvCvtColor)
import AI.CV.OpenCV.HighGui (cvLoadImage, cvSaveImage, LoadColor(..))
import AI.CV.OpenCV.ColorConversion (cv_GRAY2BGR, cv_BGR2GRAY)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.ST (runST, unsafeIOToST)
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word16)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes)
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

class HasDepth a where
    depth :: a -> Depth

instance HasChannels TriChromatic where numChannels _ = 3
instance HasChannels MonoChromatic where numChannels _ = 1
instance HasDepth Word8 where depth _ = iplDepth8u
instance HasDepth Word16 where depth _ = iplDepth16u

bytesPerPixel :: Depth -> Int
bytesPerPixel = (`div` 8) . fromIntegral . unDepth

-- |A Haskell data structure representing the information OpenCV uses
-- from an 'IplImage' struct.
{-
data HIplImage a c d = HIplImage { origin          :: Int
                                 , width           :: Int
                                 , height          :: Int
                                 , imageSize       :: Int
                                 , imageData       :: ForeignPtr d
                                 , widthStep       :: Int }
-}
data HIplImage a c d where
    HIplImage :: (HasChannels c, HasDepth d, Storable d) => 
                 Int -> Int -> Int -> Int -> ForeignPtr d -> Int -> 
                 HIplImage a c d
origin, width, height, imageSize, widthStep :: HIplImage a c d -> Int
origin (HIplImage o _ _ _ _ _) = o
width (HIplImage _ w _ _ _ _) = w
height (HIplImage _ _ h _ _ _) = h
imageSize (HIplImage _ _ _ s _ _) = s
widthStep (HIplImage _ _ _ _ _ s) = s

imageData :: HIplImage a c d -> ForeignPtr d
imageData (HIplImage _ _ _ _ d _) = d

-- |This is a way to let the type checker know that you belieave an
-- image to be tri-chromatic.
isColor :: HIplImage a TriChromatic d -> HIplImage a TriChromatic d
isColor = id

-- |This is a way to let the type checker know that you believe an
-- image to be monochromatic.
isMono :: HIplImage a MonoChromatic d -> HIplImage a MonoChromatic d
isMono = id

{-# INLINE isMono #-}
{-# INLINE isColor #-}

imgChannels :: forall a c d. HasChannels c => HIplImage a c d -> Int
imgChannels _ = numChannels (undefined::c)

-- |Return a 'V.Vector' containing the pixels that make up an
-- 8-bit-per-pixel 'HIplImage'. This does not copy the underlying
-- data!
pixels :: Storable d => HIplImage a c d -> V.Vector d
pixels img = V.unsafeFromForeignPtr (imageData img) 0 (imageSize img)

doST :: IO a -> a
doST x = runST (unsafeIOToST x)

-- |Return a 'V.Vector' containing the pixels that make up an
-- 8-bit-per-pixel 'HIplImage'. This makes a copy of the underlying
-- pixel data.
pixelsCopy :: Storable d => HIplImage a c d -> V.Vector d
pixelsCopy img = doST $ do ptr <- mallocForeignPtrBytes len
                           withForeignPtr ptr $
                              \dst -> withForeignPtr (imageData img) $
                                        \src -> copyBytes dst src len
                           return $ V.unsafeFromForeignPtr ptr 0 len
    where len = imageSize img

-- |Read an 'HIplImage' from a 'Ptr' 'IplImage'
fromPtr :: (HasChannels c, HasDepth d, Storable d) => 
           Ptr IplImage -> IO (HIplImage () c d)
fromPtr = peek . castPtr

-- |Load an 'HIplImage' from an 8-bit image file on disk. The returned
-- image will have three color channels.
fromFileColor :: String -> IO (HIplImage FreshImage TriChromatic Word8)
--fromFileColor fileName = unsafeCoerce . fromPtr =<< cvLoadImage fileName LoadColor 
fromFileColor fileName = do ptr <- cvLoadImage fileName LoadColor
                            img <- fromPtr ptr :: IO (HIplImage () TriChromatic Word8)
                            return $ unsafeCoerce img

-- |Load an 'HIplImage' from an 8-bit image file on disk. The returned
-- image will have a single color channel.
fromFileGray :: String -> IO (HIplImage FreshImage MonoChromatic Word8)
--fromFileGray fileName = unsafeCoerce . fromPtr =<< cvLoadImage fileName LoadGray
fromFileGray fileName = do ptr <- cvLoadImage fileName LoadGray
                           img <- fromPtr ptr :: IO (HIplImage () MonoChromatic Word8)
                           return $ unsafeCoerce img

toFile :: (HasChannels c, HasDepth d, Storable d) => 
          String -> HIplImage a c d -> IO ()
toFile fileName img = withHIplImage img $ \ptr -> cvSaveImage fileName ptr

-- |Prepare an 8-bit-per-pixel 'HIplImage' of the given width, height,
-- and number of color channels with an allocated pixel buffer.
mkHIplImage :: forall c d. (HasChannels c, HasDepth d, Storable d) => 
               Int -> Int -> IO (HIplImage FreshImage c d)
mkHIplImage w h = 
    do ptr <- mallocForeignPtrArray numBytes
       return $ HIplImage 0 w h numBytes ptr stride
    where numBytes = stride * h * bpp
          bpp = bytesPerPixel (depth (undefined::d))
          stride = w * (numChannels (undefined::c) :: Int)

-- |Allocate a new 'HIplImage' with the same dimensions, number of
-- color channels, and color depth as an existing HIplImage. The pixel
-- data of the original 'HIplImage' is not copied.
compatibleImage :: forall a c d. 
                   HIplImage a c d -> IO (HIplImage FreshImage c d)
compatibleImage img@(HIplImage _ _ _ _ _ _) = 
    do ptr <- mallocForeignPtrArray sz
       return $ HIplImage 0 w h sz ptr stride
    where w = width img
          h = height img
          sz = imageSize img
          stride = widthStep img

-- |Create an exact duplicate of the given HIplImage. This allocates a
-- fresh array to store the copied pixels.
duplicateImage :: forall a c d.
                  HIplImage a c d -> IO (HIplImage FreshImage c d)
duplicateImage img@(HIplImage _ _ _ _ _ _ ) =
    do fptr <- mallocForeignPtrArray sz
       withForeignPtr (imageData img) $ 
           \src -> withForeignPtr fptr $ \dst -> copyBytes dst src sz
       return $ HIplImage 0 w h sz fptr stride
    where w = width img
          h = height img
          sz = imageSize img
          stride = widthStep img

-- |Construct an 'HIplImage' from a width, a height, and a 'V.Vector'
-- of 8-bit pixel values. The new 'HIplImage' \'s pixel data is
-- shared with the supplied 'V.Vector'.
fromPixels :: forall a c. (HasChannels c, Integral a) =>
              a -> a -> V.Vector Word8 -> HIplImage () c Word8
fromPixels w h pix = if fromIntegral len == sz
                     then HIplImage 0 w' h' sz fp (w'*nc)
                     else error "Length disagreement"
    where w' = fromIntegral w
          h' = fromIntegral h
          nc = numChannels (undefined::c)
          sz = w' * h' * nc
          (fp,len) = case V.unsafeToForeignPtr (V.force pix) of
                         (fp,0,len) -> (fp,len)
                         _ -> error "fromPixels non-zero offset"

-- |Construct a fresh 'HIplImage' from a width, a height, and a
-- 'V.Vector' of 8-bit pixel values.
fromPixelsCopy :: forall a c. (Integral a, HasChannels c) =>
                  a -> a -> V.Vector Word8 -> HIplImage FreshImage c Word8
fromPixelsCopy w h pix = doST $ do fp <- copyData
                                   return $ HIplImage 0 w' h' sz fp (w'*nc)
    where w' = fromIntegral w
          h' = fromIntegral h
          nc = numChannels (undefined::c)
          sz = w' * h' * nc
          copyData = let (vfp,offset,len) = V.unsafeToForeignPtr pix
                     in do fp <- mallocForeignPtrBytes len
                           withForeignPtr vfp $
                             \src -> withForeignPtr fp $
                                       \dst -> let src' = plusPtr src offset
                                               in copyBytes dst src' len
                           return fp

-- |Helper function to explicitly type a vector of monochromatic pixel
-- data.
fromGrayPixels :: Integral a => 
                  a -> a -> V.Vector Word8 -> HIplImage () MonoChromatic Word8
fromGrayPixels w h = isMono . fromPixels w h

-- |Helper function to explicitly type a vector of trichromatic pixel
-- data.
fromColorPixels :: Integral a =>
                   a -> a -> V.Vector Word8 -> HIplImage () TriChromatic Word8
fromColorPixels w h = isColor . fromPixels w h

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'HIplImage'.
withHIplImage :: (HasChannels c, HasDepth d, Storable d) =>
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

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' that is an exact duplicate of the
-- given 'HIplImage'. Returns the duplicate 'HIplImage' after
-- performing the given action along with the result of that action.
withDuplicateImage :: (HasChannels c, HasDepth d, Storable d) => 
                      HIplImage a c d -> (Ptr IplImage -> IO b) -> 
                      (HIplImage FreshImage c d, b)
withDuplicateImage img1 f = runST $ unsafeIOToST $
                            do img2 <- duplicateImage img1
                               r <- withHIplImage img2 f
                               return (img2, r)

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' of the same dimensions as the given
-- 'HIplImage'.
withCompatibleImage :: (HasChannels c, HasDepth d, Storable d) => 
                       HIplImage a c d -> (Ptr IplImage -> IO b) -> 
                       (HIplImage FreshImage c d, b)
withCompatibleImage img1 f = runST $ unsafeIOToST $
                             do img2 <- compatibleImage img1
                                r <- withHIplImage img2 f
                                return (img2, r)

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
instance forall a c d. (HasChannels c, HasDepth d, Storable d) => 
    Storable (HIplImage a c d) where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke = error "Poking a Ptr HIplImage is unsafe."
    -- poke ptr himg = do
    --   (#poke IplImage, nSize) ptr ((#size IplImage)::Int)
    --   (#poke IplImage, ID) ptr (0::Int)
    --   (#poke IplImage, nChannels) ptr (numChannels himg)
    --   (#poke IplImage, depth) ptr (unDepth (depth himg))
    --   (#poke IplImage, dataOrder) ptr (dataOrder himg)
    --   (#poke IplImage, origin) ptr (origin himg)
    --   (#poke IplImage, width) ptr (width himg)
    --   (#poke IplImage, height) ptr (height himg)
    --   (#poke IplImage, roi) ptr nullPtr
    --   (#poke IplImage, maskROI) ptr nullPtr
    --   (#poke IplImage, imageId) ptr nullPtr
    --   (#poke IplImage, tileInfo) ptr nullPtr
    --   (#poke IplImage, imageSize) ptr (imageSize himg)
    --   withForeignPtr (imageData himg) $ \p -> (#poke IplImage, imageData) ptr p
    --   (#poke IplImage, widthStep) ptr (widthStep himg)
    --   withForeignPtr (imageDataOrigin himg) $ 
    --     \p ->(#poke IplImage, imageDataOrigin) ptr p
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




                    
