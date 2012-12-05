{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, 
             FlexibleInstances, DataKinds, KindSignatures #-}
-- |Functions for working with 'HIplImage's.
module OpenCV.Core.ImageUtil
    (isColor, isMono, imgChannels, withPixelVector, pixelVector, 
     peekIpl, fromFileColor, fromFileGray, fromPGM16, toFile, 
     compatibleImage, duplicateImage, fromPixels, unsafePixelVector,
     withImagePixels, fromGrayPixels, fromColorPixels,
     withDuplicateImage, withCompatibleImage, setROI, resetROI,
     mkImage, mallocImage, numPixels, blackImage, Image(..), 
     ROIEnabled(..), withIplImage, Channels(..), 
     GrayImage, GrayImage16, GrayImage16S, GrayImageF, ColorImage,
     withDuplicatePixels, c_cvSetImageROI, c_cvResetImageROI,
     HasDepth(..), CvScalarT, AsCvScalar(..), ScalarOK,
     colorDepth, UpdateROI, SingI, withDuplicateRGBPixels,
     ByteOrFloat, getRect, fromFile, unsafeWithHIplImage,
     duplicateImagePtr, compatibleImagePtr, compatibleImagePtrPtr) where
import OpenCV.Core.CxCore (IplImage, cvFree, cvFreePtr, createImageF,
                           cloneImageF, cvCreateImage, CvSize(..),
                           getNumChannels, getDepth, cvGetSize)
import OpenCV.Core.HighGui (cvLoadImage, cvSaveImage, LoadColor(..))
import OpenCV.Core.Image
import OpenCV.Color
import Control.Applicative
import Control.Arrow (second, (***))
import Control.Monad (when, unless, join)
import Data.Int (Int16)
import Data.Proxy
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Singletons (SingI)
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.Directory (doesFileExist)
import System.IO (openFile, hGetLine, hGetBuf, hClose, hSetBinaryMode, 
                  IOMode(..))
import System.IO.Unsafe

-- |Some operations are restricted to bytes or floats.
class (HasDepth a, Num a) => ByteOrFloat a where
instance ByteOrFloat Word8 where
instance ByteOrFloat Float where

-- |Grayscale 8-bit (per-pixel) image type.
type GrayImage = Image Monochromatic Word8 NoROI

-- |Grayscale unsigned 16-bit (per-pixel) image type.
type GrayImage16 = Image Monochromatic Word16 NoROI

-- |Grayscale signed 16-bit (per-pixel) image type.
type GrayImage16S = Image Monochromatic Int16 NoROI

-- |Grayscale single precision floating point image type.
type GrayImageF = Image Monochromatic Float NoROI

-- |Color 8-bit (per-color) image type.
type ColorImage = Image Trichromatic Word8 NoROI

-- |This is a way to let the type checker know that you belieave an
-- image to be tri-chromatic. If your image type can't be inferred any
-- other way, this is an alternative to adding a type annotation.
isColor :: Image Trichromatic d r -> Image Trichromatic d r
isColor = id

-- |This is a way to let the type checker know that you believe an
-- image to be monochromatic. If your image type can't be inferred any
-- other way, this is an alternative to adding a type annotation.
isMono :: Image Monochromatic d r -> Image Monochromatic d r
isMono = id

{-# INLINE isMono #-}
{-# INLINE isColor #-}

-- |Return the number of color channels a 'HIplImage' has as a runtime
-- value.
imgChannels :: forall c d r. Image c d r -> Int
imgChannels Image{} = fromIntegral $ numChannels (Proxy::Proxy c)

-- |Return the number of bytes per pixel color component of an
-- 'HIplImage'.
colorDepth :: forall c d r. Image c d r -> Int
colorDepth Image{} = bytesPerPixel (undefined::d)

-- |The number of pixels in the image: @width img * height img@.
numPixels :: Image c d r -> Int
numPixels = fromIntegral . ((*) <$> width <*> height)

-- |Apply the supplied function to a 'V.Vector' containing the pixels
-- that make up an 'HIplImage'. This does not copy the underlying
-- data.
withImagePixels :: Image c d NoROI -> (V.Vector d -> r) -> r
withImagePixels img@Image{} f = f $ V.unsafeFromForeignPtr (imageData img) 0 n
    where n = fromIntegral (imageSize img) `div` colorDepth img

-- |Apply the supplied function to a mutable 'VM.IOVector' containing
-- a copy of the pixel data from the input image. Returns the new
-- image and any result of the 'IO' action.
withDuplicatePixels :: Image c d NoROI -> (VM.IOVector d -> IO r) -> 
                       IO (Image c d NoROI, r)
withDuplicatePixels img1@Image{} f = do img2 <- duplicateImage img1
                                        let ptr = imageDataOrigin img2
                                        r <- f $ VM.unsafeFromForeignPtr0 ptr n
                                        return (img2, r)
  where n = numPixels img1 * imgChannels img1

-- |Specialization of 'withDuplicatePixels' to ease the common case of
-- dealing with 8 bit triples for each pixel.
withDuplicateRGBPixels :: Image Trichromatic Word8 NoROI -> 
                          (VM.IOVector RGB8 -> IO r) -> 
                          IO (Image Trichromatic Word8 NoROI, r)
withDuplicateRGBPixels img1 f = do img2 <- duplicateImage img1
                                   let ptr = castForeignPtr $ 
                                             imageDataOrigin img2
                                   r <- f $ VM.unsafeFromForeignPtr0 ptr n
                                   return (img2, r)
  where n = numPixels img1

-- |Return a 'V.Vector' containing a copy of the pixels that make up
-- an 'Image'.
pixelVector :: forall c d. (HasDepth d, Storable d) => 
               Image c d NoROI -> V.Vector d
pixelVector img = unsafePerformIO $ 
                  do ptr <- mallocForeignPtrBytes len
                     withForeignPtr ptr $ \dst -> 
                       withForeignPtr (imageData img) $ \src -> 
                         copyBytes dst src len
                     return $ V.unsafeFromForeignPtr0 ptr n
    where len = fromIntegral $ imageSize img
          n = len `quot` bytesPerPixel (undefined::d)
{-# NOINLINE pixelVector #-}

-- |Return a 'V.Vector' pointing to the pixels that make up an
-- 'Image'.
unsafePixelVector :: forall c d. (HasDepth d, Storable d) => 
                     Image c d NoROI -> V.Vector d
unsafePixelVector img = V.unsafeFromForeignPtr0 (imageData img) n
  where n = imageSize img `quot` bytesPerPixel (undefined::d)

-- Ensure that a file exists.
checkFile :: FilePath -> IO ()
checkFile f = do e <- doesFileExist f
                 unless e (error $ "Can't find "++f)

-- |Load a color 'HIplImage' from an 8-bit image file. If the image
-- file is grayscale, it will be converted to color.
fromFileColor :: FilePath -> IO (Image Trichromatic Word8 NoROI)
fromFileColor fileName = 
  do checkFile fileName
     ptr <- cvLoadImage fileName LoadColor
     img <- peekIpl ptr :: IO (Image Trichromatic Word8 NoROI)
     addForeignPtrFinalizer cvFreePtr (imageDataOrigin img)
     freeROI ptr
     cvFree ptr
     return img

-- |Load a grayscale 'HIplImage' from an 8-bit image file. If the
-- image file is color, it will be converted to grayscale.
fromFileGray :: FilePath -> IO (Image Monochromatic Word8 NoROI)
fromFileGray fileName = 
  do checkFile fileName
     ptr <- cvLoadImage fileName LoadGray
     img <- peekIpl ptr :: IO (Image Monochromatic Word8 NoROI)
     addForeignPtrFinalizer cvFreePtr (imageDataOrigin img)
     return img

-- |If the type of a loaded image is known (e.g. by a type annotation,
-- or usage of a narrowly typed functions), then we can automatically
-- dispatch to the proper image loading routine.
class LoadableFormat (c::Channels) d where
  loadFormat :: (Proxy c,d) -> FilePath -> IO (Image c d NoROI)

instance LoadableFormat Monochromatic Word8 where
  loadFormat _ = fromFileGray

instance LoadableFormat Trichromatic Word8 where
  loadFormat _ = fromFileColor

instance LoadableFormat Monochromatic Word16 where
  loadFormat _ = fromPGM16

-- |An overloaded image file loader. The number of color channels and
-- color depth parts of the returned image's type must be inferrable
-- as they control how the image file is loaded.
fromFile :: forall c d. LoadableFormat c d => FilePath -> IO (Image c d NoROI)
fromFile = loadFormat (Proxy::Proxy c, undefined::d)

-- |Load a grayscale 'HIplImage' from a 16-bit image file. NOTE:
-- OpenCV (as of v2.2) does not correctly handle 16-bit PGM loading,
-- so this 16bpp loader is restricted to PGM. This loading routine
-- converts from Most Significant Byte first (MSB) byte ordering (as
-- per the PGM spec) to LSB byte ordering for x86 compatibility.
fromPGM16 :: FilePath -> IO (Image Monochromatic Word16 NoROI)
fromPGM16 fileName = 
  do checkFile fileName
     h <- openFile fileName ReadMode
     magic <- hGetLine h
     when (magic /= "P5") (hClose h >> 
                           error (fileName ++" is not a PGM"))
     (width, height) <- fmap ((read***read). second tail . break (==' '))
                             (hGetLine h) :: IO (Int,Int)
     maxCol <- hGetLine h
     when (maxCol /= "65535") (hClose h >>
                               error (fileName ++" is not 16-bit"))
     let numBytes = fromIntegral $ width*height*2
     fp <- mallocForeignPtrArray numBytes
     hSetBinaryMode h True
     withForeignPtr fp $ \ptr' ->
       do let ptr = castPtr ptr' :: Ptr Word8
          n <- hGetBuf h ptr numBytes
          when (n /= numBytes) (hClose h >> 
                                error (fileName ++" unexpected EOF"))
          let swapBytes !offset
                | offset == numBytes = return ()
                | otherwise = do temp1 <- peekByteOff ptr offset :: IO Word8
                                 temp2 <- peekByteOff ptr (offset+1) :: IO Word8
                                 pokeByteOff ptr offset temp2
                                 pokeByteOff ptr (offset+1) temp1
                                 swapBytes (offset+2)
          swapBytes 0
     hClose h
     return $ mkImage width height fp

-- |Save an image to the specified file.
toFile :: HasDepth d => FilePath -> Image c d r -> IO ()
toFile fileName img = withIplImage img $ \ptr -> cvSaveImage fileName ptr

-- |Allocate a new 'Image' with the same dimensions, number of color
-- channels, and color depth as an existing 'Image'. The pixel data of
-- the original 'Image' is not copied.
compatibleImage :: Image c d r -> IO (Image c d r)
compatibleImage img@Image{} = updateROI (roi img) <$> 
                              mallocImage (width img) (height img)

-- |Allocate a new 'IplImage' with the same dimensions, number of
-- color channels, and color depth as an existing 'HIplImage'. The
-- pixel data of the original 'HIplImage' is not copied.
compatibleImagePtr :: forall c d r. Image c d r -> IO (ForeignPtr IplImage)
compatibleImagePtr img@Image{} = createImageF (CvSize w' h') nc d
    where w' = fromIntegral . width $ img
          h' = fromIntegral . height $ img
          nc = fromIntegral . imgChannels $ img
          d = depth (undefined::d)

compatibleImagePtrPtr :: Ptr IplImage -> IO (Ptr IplImage)
compatibleImagePtrPtr = 
    join . (liftA3 cvCreateImage <$> cvGetSize <*> getNumChannels <*> getDepth)

-- |Create an exact duplicate of the given 'Image'. This allocates a
-- fresh array to store the copied pixels.
duplicateImage :: Image c d r -> IO (Image c d r)
duplicateImage img@Image{} = 
  do img' <- updateROI (roi img) <$> mallocImage (width img) (height img)
     withForeignPtr (imageData img) $ \src ->
       withForeignPtr (imageData img') $ \dst ->
         copyBytes dst src (fromIntegral $ imageSize img)
     return img'

-- |Clone an 'Image', returning the 'Ptr' 'IplImage' underlying
-- the clone.
duplicateImagePtr :: Image c d r -> IO (ForeignPtr IplImage)
duplicateImagePtr = flip withIplImage cloneImageF

-- |Pass the given function an 'Image' constructed from a width, a
-- height, and a 'V.Vector' of pixel values. The new 'Image'\'s pixel
-- data is shared with the supplied 'V.Vector'. In other words, this
-- lets the user apply a function on 'Image's to a 'V.Vector' of pixel
-- data.
withPixelVector :: forall a c d r. (HasDepth d, Integral a, SingI c) =>
                   a -> a -> (Image c d NoROI -> r) -> V.Vector d -> r
withPixelVector w h f pix = if len == sz
                            then f $ mkImage w h fp
                            else error "Length disagreement"
    where sz = fromIntegral (w * h) * numChannels (Proxy::Proxy c)
          (fp,len) = case V.unsafeToForeignPtr (V.force pix) of
                         (fp,0,len) -> (fp,len)
                         _ -> error "fromPixels non-zero offset"

-- |Construct a fresh 'Image' from a width, a height, and a 'V.Vector'
-- of pixel values.
fromPixels :: forall a c d. (Integral a, SingI c, HasDepth d) =>
              a -> a -> V.Vector d -> Image c d NoROI
fromPixels w h pix = unsafePerformIO $ 
                     do fp <- copyData
                        return $ mkImage w h fp
    where copyData = let (vfp,len) = V.unsafeToForeignPtr0 pix
                     in do fp <- mallocForeignPtrBytes len
                           withForeignPtr vfp $ \src -> 
                             withForeignPtr fp $ \dst -> 
                               copyBytes dst src len
                           return fp
{-# INLINE [0] fromPixels #-}

-- |Helper function to explicitly type a vector of monochromatic pixel
-- data. Parameters are the output image's width, height, and pixel
-- content.
fromGrayPixels :: (HasDepth d, Integral a) => 
                  a -> a -> V.Vector d -> Image Monochromatic d NoROI
fromGrayPixels w h = isMono . fromPixels w h

-- |Helper function to explicitly type a vector of interleaved
-- trichromatic pixel data. Parameters are the output image's width,
-- height, and pixel content.
fromColorPixels :: (HasDepth d, Integral a) =>
                   a -> a -> V.Vector d -> Image Trichromatic d NoROI
fromColorPixels w h = isColor . fromPixels w h

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'Image' that is an exact duplicate of the given
-- 'Image'. Returns the duplicate 'Image' after performing the given
-- action along with the result of that action.
withDuplicateImage :: Image c d r -> (Ptr IplImage -> IO b) -> 
                      IO (Image c d r, b)
withDuplicateImage img1 f = do img2 <- duplicateImage img1
                               r <- withIplImage img2 f
                               return (img2, r)

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'Image' of the same dimensions as the given
-- 'Image'.
withCompatibleImage :: Image c d r -> (Ptr IplImage -> IO b) -> 
                       IO (Image c d r, b)
withCompatibleImage img1 f = do img2 <- compatibleImage img1
                                r <- withIplImage img2 f
                                return (img2, r)

unsafeWithHIplImage :: Image c d r -> (Ptr IplImage -> a) -> a
unsafeWithHIplImage img f = unsafePerformIO $ withIplImage img (return . f)

-- |Extract a rectangular region of interest from an image. Returns a
-- new image whose pixel data is copied from the given rectangle of
-- the source image. Parameters are the upper-left corner of the
-- rectangle in image coordinates, the (width,height) of the rectangle
-- in pixels, and the source 'Image'.
getRect :: forall c d r. 
           (Int,Int) -> (Int,Int) -> Image c d r -> IO (Image c d NoROI)
getRect (rx,ry) (rw,rh) src@Image{} = 
    do img <- mallocImage rw rh :: IO (Image c d NoROI)
       withForeignPtr (imageData img) $ \dst ->
         withForeignPtr (imageData src) $ \src ->
           mapM_ (\(dOff, sOff) -> copyBytes (plusPtr dst dOff) 
                                             (plusPtr src sOff)
                                             rowLen)
                 (zip [0,rowLen..rw*rh*bpp-1] [start,start+stride..])
       return img
    where stride = fromIntegral $ widthStep src
          start = stride*ry + rx*bpp
          bpp = imgChannels src * colorDepth src
          rowLen = rw*bpp
