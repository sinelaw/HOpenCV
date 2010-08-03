{-# LANGUAGE ForeignFunctionInterface #-}
module AI.CV.OpenCV.HIplImage where
import AI.CV.OpenCV.CxCore (IplImage,Depth(..),iplDepth8u,createImageF,
                            CvSize(..))
import AI.CV.OpenCV.HighGui (cvLoadImage, cvSaveImage, LoadColor)
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
data HIplImage = HIplImage { numChannels     :: Int
                           , depth           :: Depth
                           , dataOrder       :: Int
                           , origin          :: Int
                           , width           :: Int
                           , height          :: Int
                           , imageSize       :: Int
                           , imageData       :: ForeignPtr Word8
                           , widthStep       :: Int
                           , imageDataOrigin :: ForeignPtr Word8 }

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

-- |Load an 'HIplImage' from an image file on disk. The first argument
-- is the name of the file to load. The second argument determines
-- the desired color format of the image.
fromFile :: String -> LoadColor -> IO HIplImage
fromFile fileName col = fromPtr =<< cvLoadImage fileName col

toFile :: String -> HIplImage -> IO ()
toFile fileName img = withHIplImage img $ \ptr -> cvSaveImage fileName ptr

-- |Prepare an 8-bit-per-pixel 'HIplImage' of the given width, height,
-- and number of color channels with an allocated pixel buffer.
mkHIplImage :: Int -> Int -> Int -> IO HIplImage
mkHIplImage w h numChan = 
--     do fp <- createImageF (CvSize w' h') numChan' iplDepth8u
--        withForeignPtr fp $ \p -> peek (castPtr p)
--     where w' = fromIntegral w
--           h' = fromIntegral h
--           numChan' = fromIntegral numChan
    do ptr <- mallocArray numBytes >>= newForeignPtr finalizerFree
       return $ HIplImage numChan iplDepth8u 0 0 w h numBytes ptr stride ptr
    where numBytes = stride * h
          stride = w * numChan

-- |Allocate a new 'HIplImage' with the same dimensions, number of
-- color channels, and color depth as an existing HIplImage. The pixel
-- data of the original 'HIplImage' is not copied.
compatibleImage :: HIplImage -> IO HIplImage
compatibleImage img = 
    do ptr <- mallocArray sz >>= newForeignPtr finalizerFree
       return $ HIplImage nc d order 0 w h sz ptr stride ptr
    where w = width img
          h = height img
          nc = numChannels img
          d = depth img
          order = dataOrder img
          sz = imageSize img
          stride = widthStep img

-- |Create an exact duplicate of the given HIplImage. This allocates a
-- fresh array to store the copied pixels.
duplicateImage :: HIplImage -> IO HIplImage
duplicateImage img =
    do ptr <- mallocArray sz
       withForeignPtr (imageData img) $ 
           \src -> copyArray ptr src sz
       fptr <- newForeignPtr finalizerFree ptr
       return $ HIplImage nc d 0 0 w h sz fptr stride fptr
    where w = width img
          h = height img
          nc = numChannels img
          d = depth img
          sz = imageSize img
          stride = widthStep img

{-# NOINLINE fromPixels #-}
-- |Construct an 'HIplImage' from a width, a height, and a 'V.Vector'
-- of 8-bit pixel values. The new 'HIplImage' \'s pixel data is shared
-- with the supplied 'V.Vector'.
fromPixels :: Integral a => a -> a -> V.Vector Word8 -> HIplImage
fromPixels w h pix = unsafePerformIO $ 
                     V.unsafeWith pix $
                         \p -> do fp <- newForeignPtr_ p
                                  return $ HIplImage nc iplDepth8u 0 0 w' h'
                                                     sz fp stride fp
    where nc = if V.length pix == w' * h' then 1 else 3
          w' = fromIntegral w
          h' = fromIntegral h
          sz = w' * h' * nc
          stride = w' * nc

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying the given 'HIplImage'.
withHIplImage :: HIplImage -> (Ptr IplImage -> IO a) -> IO a
withHIplImage img f = alloca $ \p -> poke p img >> f (castPtr p)

{-# NOINLINE withDuplicateImage #-}
-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' that is an exact duplicate of the
-- given 'HIplImage'.
withDuplicateImage :: HIplImage -> (Ptr IplImage -> IO a) -> HIplImage
withDuplicateImage img1 f = unsafePerformIO $
                            do img2 <- duplicateImage img1
                               _ <- withHIplImage img2 f
                               return img2

{-# NOINLINE withCompatibleImage #-}
-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' of the same dimensions as the given
-- 'HIplImage'.
withCompatibleImage :: HIplImage -> (Ptr IplImage -> IO a) -> HIplImage
withCompatibleImage img1 f = unsafePerformIO $ 
                             do img2 <- compatibleImage img1
                                _ <- withHIplImage img2 f
                                return img2

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
      (#poke IplImage, nSize) ptr ((#size IplImage)::Int)
      (#poke IplImage, ID) ptr (0::Int)
      (#poke IplImage, nChannels) ptr (numChannels himg)
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
      numChannels' <- (#peek IplImage, nChannels) ptr
      depth' <- Depth <$> (#peek IplImage, depth) ptr
      dataOrder' <- (#peek IplImage, dataOrder) ptr
      origin' <- (#peek IplImage, origin) ptr
      width' <- (#peek IplImage, width) ptr
      height' <- (#peek IplImage, height) ptr
      imageSize' <- (#peek IplImage, imageSize) ptr
      imageData' <- (#peek IplImage, imageData) ptr >>= newForeignPtr_
      widthStep' <- (#peek IplImage, widthStep) ptr
      imageDataOrigin' <- (#peek IplImage, imageDataOrigin) ptr >>= 
                          newForeignPtr_
      return $ HIplImage numChannels' depth' dataOrder' origin' 
                         width' height' imageSize' imageData' widthStep' 
                         imageDataOrigin'




                    
