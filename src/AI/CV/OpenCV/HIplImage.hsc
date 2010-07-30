{-# LANGUAGE ForeignFunctionInterface #-}
module AI.CV.OpenCV.HIplImage where
import AI.CV.OpenCV.CxCore (IplImage,Depth(..),iplDepth8u)
import AI.CV.OpenCV.CV (cvErode, cvDilate)
import Control.Applicative ((<$>))
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word16)
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca, finalizerFree)
import Foreign.Marshal.Array (mallocArray, copyArray)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

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

-- |A Haskell data structure representing the information OpenCV uses
-- from an 'IplImage' struct.
data HIplImage = HIplImage { nSize           :: Int
                           , nChannels       :: Int
                           , depth           :: Depth
                           , dataOrder       :: Int
                           , origin          :: Int
                           , width           :: Int
                           , height          :: Int
--                         , roi             :: Ptr ()
                           , imageSize       :: Int 
                           , imageData       :: ForeignPtr Word8
                           , widthStep       :: Int
                           , imageDataOrigin :: ForeignPtr Word8 }

-- |Return a 'V.Vector' containing a copy of the pixels that make up
-- the 8-bit-per-pixel 'HIplImage'.
pixelsCopy :: HIplImage -> IO (V.Vector Word8)
pixelsCopy img = do dst <- mallocArray sz
                    withForeignPtr src $ \src' -> copyArray dst src' sz
                    fptr <- newForeignPtr finalizerFree dst
                    return $ V.unsafeFromForeignPtr fptr 0 sz
    where sz = imageSize img
          src = case depth img of
                  Depth 8 -> imageData img
                  x -> error $ "Pixel depth must be 8, "++show x++
                               " is not supported"

-- |Return a 'V.Vector' containing the pixels that make up an
-- 8-bit-per-pixel 'HIplImage'. This does not copy the underlying
-- data!
pixels :: HIplImage -> V.Vector Word8
pixels img = V.unsafeFromForeignPtr ptr 0 (imageSize img)
    where ptr = case depth img of
                  Depth 8 -> imageData img
                  x -> error $ "Pixel depth must be 8, "++show x++
                               " is not supported"


-- |Return a 'V.Vector' containing the pixels that make up the
-- 16-bit-per-pixel 'HIplImage'.
pixels16 :: HIplImage -> V.Vector Word16
pixels16 img = V.unsafeFromForeignPtr ptr 0 (imageSize img)
    where ptr = case depth img of
                  Depth 16 -> castForeignPtr (imageData img)
                  x -> error $ "Pixel depth must be 16, "++show x++
                               " is not supported"

-- |Read an 'HIplImage' from a 'Ptr' 'IplImage'
fromPtr :: Ptr IplImage -> IO HIplImage
fromPtr = peek . castPtr

-- |Prepare an 8-bit-per-pixel 'HIplImage' of the given width, height,
-- and number of color channels with an allocated pixel buffer.
mkHIplImage :: Int -> Int -> Int -> IO HIplImage
mkHIplImage w h numChan = do buffer <- mallocArray numBytes
                             ptr <- newForeignPtr finalizerFree buffer
                             return $ HIplImage (#size IplImage)
                                                numChan
                                                iplDepth8u 0 0 w h
                                                numBytes
                                                ptr
                                                (w*numChan)
                                                ptr
    where numBytes = w * h * numChan

-- |Allocate a new 'HIplImage' with the same dimensions, number of
-- color channels, and color depth as an existing HIplImage. The pixel
-- data of the original 'HIplImage' is not copied.
compatibleImage :: HIplImage -> IO HIplImage
compatibleImage img = 
    do ptr <- newForeignPtr finalizerFree =<< mallocArray sz
       return $ HIplImage (#size IplImage) nc d 0 0 w h sz ptr stride ptr
    where w = width img
          h = height img
          nc = nChannels img
          d = depth img
          sz = imageSize img
          stride = widthStep img

-- |Construct an 'HIplImage' from a width, a height, and a 'V.Vector'
-- of 8-bit pixel values. The new 'HIplImage' \'s pixel data is shared
-- with the supplied 'V.Vector'.
fromPixels :: Integral a => a -> a -> V.Vector Word8 -> HIplImage
fromPixels w h pix = unsafePerformIO $ 
                     V.unsafeWith pix $
                         \p -> do fp <- newForeignPtr_ p
                                  return $ HIplImage (#size IplImage) nc 
                                                     iplDepth8u 0 0 w' h'
                                                     sz fp stride fp
    where sz = V.length pix
          nc = if sz == w'*h' then 1 else 3
          stride = w' * nc
          w' = fromIntegral w
          h' = fromIntegral h

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'HIplImage'.
withHIplImage :: HIplImage -> (Ptr IplImage -> IO a) -> IO a
withHIplImage img f = alloca $ \p -> poke p img >> f (castPtr p)

-- |Erode an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
erode :: HIplImage -> Int -> HIplImage
erode img n = unsafePerformIO $ 
              do destImg <- compatibleImage img
                 withHIplImage img (\src -> withHIplImage destImg $
                                            \dst -> cvErode src dst n')
                 return destImg
    where n' = fromIntegral n

-- |Dilate an 'HIplImage' with a 3x3 structuring element for the
-- specified number of iterations.
dilate :: HIplImage -> Int -> HIplImage
dilate img n = unsafePerformIO $ 
               do destImg <- compatibleImage img
                  withHIplImage img (\src -> withHIplImage destImg $
                                             \dst -> cvDilate src dst n')
                  return destImg
    where n' = fromIntegral n

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
instance Storable HIplImage where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke ptr himg = do
      (#poke IplImage, nSize) ptr (nSize himg)
      (#poke IplImage, ID) ptr (0::Int)
      (#poke IplImage, nChannels) ptr (nChannels himg)
      (#poke IplImage, depth) ptr (unDepth (depth himg))
      (#poke IplImage, dataOrder) ptr (dataOrder himg)
      (#poke IplImage, origin) ptr (origin himg)
      (#poke IplImage, width) ptr (width himg)
      (#poke IplImage, height) ptr (height himg)
      (#poke IplImage, roi) ptr nullPtr
      (#poke IplImage, maskROI) ptr nullPtr
      (#poke IplImage, imageId) ptr nullPtr
      (#poke IplImage, tileInfo) ptr nullPtr
      (#poke IplImage, imageSize) ptr (imageSize himg)
      withForeignPtr (imageData himg) $ \p -> (#poke IplImage, imageData) ptr p
      (#poke IplImage, widthStep) ptr (widthStep himg)
      withForeignPtr (imageDataOrigin himg) $ 
         \p ->(#poke IplImage, imageDataOrigin) ptr p
    peek ptr = do
      nSize' <- (#peek IplImage, nSize) ptr
      nChannels' <- (#peek IplImage, nChannels) ptr
      depth' <- Depth <$> (#peek IplImage, depth) ptr
      dataOrder' <- (#peek IplImage, dataOrder) ptr
      origin' <- (#peek IplImage, origin) ptr
      width' <- (#peek IplImage, width) ptr
      height' <- (#peek IplImage, height) ptr
      imageSize' <- (#peek IplImage, imageSize) ptr
      imageData' <- (#peek IplImage, imageData) ptr >>= newForeignPtr_
      widthStep' <- (#peek IplImage, widthStep) ptr
      imageDataOrigin' <- (#peek IplImage, imageDataOrigin) ptr >>= newForeignPtr_
      return $ HIplImage nSize' nChannels' depth' dataOrder' origin' 
                         width' height' imageSize' imageData' widthStep' 
                         imageDataOrigin'



                    
