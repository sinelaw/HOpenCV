{-# LANGUAGE ForeignFunctionInterface #-}
module AI.CV.OpenCV.HIplImage where
import AI.CV.OpenCV.CxCore (IplImage)
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word16)
import Foreign.C.Types
import Foreign.ForeignPtr
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
                           , depth           :: Int
                           , dataOrder       :: Int
                           , origin          :: Int
                           , width           :: Int
                           , height          :: Int
--                         , roi             :: Ptr ()
                           , imageSize       :: Int 
                           , imageData       :: Ptr Word8
                           , widthStep       :: Int
                           , imageDataOrigin :: Ptr Word8 }

-- |Return a 'V.Vector' containing the pixels that make up the
-- 8-bit-per-pixel 'HIplImage'.
pixels :: HIplImage -> V.Vector Word8
pixels img = V.unsafeFromForeignPtr fptr 0 (imageSize img)
    where fptr = unsafePerformIO $ newForeignPtr_ ptr
          ptr = case depth img of
                  8 -> imageData img
                  x -> error $ "Pixel depth must be 8, "++show x++
                               " is not supported"

-- |Return a 'V.Vector' containing the pixels that make up the
-- 16-bit-per-pixel 'HIplImage'.
pixels16 :: HIplImage -> V.Vector Word16
pixels16 img = V.unsafeFromForeignPtr fptr 0 (imageSize img)
    where fptr = unsafePerformIO $ newForeignPtr_ ptr
          ptr = case depth img of
                  16 -> castPtr (imageData img) :: Ptr Word16
                  x -> error $ "Pixel depth must be 16, "++show x++
                               " is not supported"

-- |Read an 'HIplImage' from a 'Ptr' 'IplImage'
fromPtr :: Ptr IplImage -> IO HIplImage
fromPtr = peek . castPtr

instance Storable HIplImage where
    sizeOf _ = (#size IplImage)
    alignment _ = alignment (undefined :: CDouble)
    poke ptr himg = do
      (#poke IplImage, nSize) ptr (nSize himg)
      (#poke IplImage, ID) ptr (0::Int)
      (#poke IplImage, nChannels) ptr (nChannels himg)
      (#poke IplImage, depth) ptr (depth himg)
      (#poke IplImage, dataOrder) ptr (dataOrder himg)
      (#poke IplImage, origin) ptr (origin himg)
      (#poke IplImage, width) ptr (width himg)
      (#poke IplImage, height) ptr (height himg)
      (#poke IplImage, roi) ptr nullPtr
      (#poke IplImage, maskROI) ptr nullPtr
      (#poke IplImage, imageId) ptr nullPtr
      (#poke IplImage, tileInfo) ptr nullPtr
      (#poke IplImage, imageSize) ptr (imageSize himg)
      (#poke IplImage, imageData) ptr (imageData himg)
      (#poke IplImage, widthStep) ptr (widthStep himg)
      (#poke IplImage, imageDataOrigin) ptr (imageDataOrigin himg)
    peek ptr = do
      nSize' <- (#peek IplImage, nSize) ptr
      nChannels' <- (#peek IplImage, nChannels) ptr
      depth' <- (#peek IplImage, depth) ptr
      dataOrder' <- (#peek IplImage, dataOrder) ptr
      origin' <- (#peek IplImage, origin) ptr
      width' <- (#peek IplImage, width) ptr
      height' <- (#peek IplImage, height) ptr
      imageSize' <- (#peek IplImage, imageSize) ptr
      imageData' <- (#peek IplImage, imageData) ptr
      widthStep' <- (#peek IplImage, widthStep) ptr
      imageDataOrigin' <- (#peek IplImage, imageDataOrigin) ptr
      return $ HIplImage nSize' nChannels' depth' dataOrder' origin' 
                         width' height' imageSize' imageData' widthStep' 
                         imageDataOrigin'



                    
