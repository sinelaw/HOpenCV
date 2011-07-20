{-# LANGUAGE ScopedTypeVariables, BangPatterns, MultiParamTypeClasses, 
             FlexibleInstances #-}
-- |Functions for working with 'HIplImage's.
module AI.CV.OpenCV.Core.HIplUtil
    (isColor, isMono, imgChannels, withPixelVector, pixels,
     fromPtr, fromFileColor, fromFileGray, fromPGM16, toFile, 
     compatibleImage, duplicateImage, fromPixels,
     withImagePixels, fromGrayPixels, fromColorPixels,
     withDuplicateImage, withCompatibleImage, 
     mkHIplImage, width, height, mkBlackImage, HIplImage, NoROI, HasROI,
     withHIplImage, MonoChromatic, TriChromatic, HasChannels, ImgBuilder(..),
     GrayImage, GrayImage16, ColorImage, c_cvSetImageROI, c_cvResetImageROI,
     HasDepth(..), HasScalar(..), IsCvScalar(..), colorDepth,
     ByteOrFloat, getRect, imageData, fromFile, unsafeWithHIplImage,
     duplicateImagePtr, compatibleImagePtr, compatibleImagePtrPtr) where
import AI.CV.OpenCV.Core.CxCore (IplImage, cvFree, cvFreePtr, createImageF, 
                                 CvSize(..), cloneImageF, cvCreateImage, 
                                 getNumChannels, getDepth, cvGetSize)
import AI.CV.OpenCV.Core.HighGui (cvLoadImage, cvSaveImage, LoadColor(..))
import AI.CV.OpenCV.Core.HIplImage
import Control.Applicative
import Control.Arrow (second, (***))
import Control.Monad (when, unless, join)
import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.Directory (doesFileExist)
import System.IO (openFile, hGetLine, hGetBuf, hClose, hSetBinaryMode, 
                  IOMode(..))
import System.IO.Unsafe

-- |Grayscale 8-bit (per-pixel) image type.
type GrayImage = HIplImage MonoChromatic Word8 NoROI

-- |Grayscale 16-bit (per-pixel) image type.
type GrayImage16 = HIplImage MonoChromatic Word16 NoROI

-- |Color 8-bit (per-color) image type.
type ColorImage = HIplImage TriChromatic Word8 NoROI

-- |This is a way to let the type checker know that you belieave an
-- image to be tri-chromatic. If your image type can't be inferred any
-- other way, this is an alternative to adding a type annotation.
isColor :: HIplImage TriChromatic d r -> HIplImage TriChromatic d r
isColor = id

-- |This is a way to let the type checker know that you believe an
-- image to be monochromatic. If your image type can't be inferred any
-- other way, this is an alternative to adding a type annotation.
isMono :: HIplImage MonoChromatic d r -> HIplImage MonoChromatic d r
isMono = id

{-# INLINE isMono #-}
{-# INLINE isColor #-}

-- |Return the number of color channels a 'HIplImage' has as a runtime
-- value.
imgChannels :: forall c d r. HasChannels c => HIplImage c d r -> Int
imgChannels _ = fromIntegral $ numChannels (undefined::c)

-- |Return the number of bytes per pixel color component of an
-- 'HIplImage'.
colorDepth :: forall c d r. HasDepth d => HIplImage c d r -> Int
colorDepth _ = bytesPerPixel (undefined::d)

-- |Apply the supplied function to a 'V.Vector' containing the pixels
-- that make up an 'HIplImage'. This does not copy the underlying
-- data.
withImagePixels :: HasDepth d => HIplImage c d NoROI -> (V.Vector d -> r) -> r
withImagePixels img f = f $ V.unsafeFromForeignPtr (imageData img) 0 n
    where n = fromIntegral (imageSize img) `div` colorDepth img

-- |Return a 'V.Vector' containing a copy of the pixels that make up a
-- 'HIplImage'.
pixels :: Storable d => HIplImage c d NoROI -> V.Vector d
pixels img = unsafePerformIO $ 
             do ptr <- mallocForeignPtrBytes len
                withForeignPtr ptr $ \dst -> 
                    withForeignPtr (imageData img) $ \src -> 
                        copyBytes dst src len
                return $ V.unsafeFromForeignPtr ptr 0 len
    where len = fromIntegral $ imageSize img
{-# NOINLINE pixels #-}

-- |Read a 'HIplImage' from a 'Ptr' 'IplImage'
fromPtr :: (HasChannels c, HasDepth d, ImgBuilder r) => 
           Ptr IplImage -> IO (HIplImage c d r)
fromPtr = peek . castPtr

-- Ensure that a file exists.
checkFile :: FilePath -> IO ()
checkFile f = do e <- doesFileExist f
                 unless e (error $ "Can't find "++f)

-- |Load a color 'HIplImage' from an 8-bit image file. If the image
-- file is grayscale, it will be converted to color.
fromFileColor :: FilePath -> IO (HIplImage TriChromatic Word8 NoROI)
fromFileColor fileName = 
  do checkFile fileName
     ptr <- cvLoadImage fileName LoadColor
     img <- fromPtr ptr :: IO (HIplImage TriChromatic Word8 NoROI)
     addForeignPtrFinalizer cvFreePtr (imageDataOrigin img)
     freeROI ptr
     cvFree ptr
     return img

-- |Load a grayscale 'HIplImage' from an 8-bit image file. If the
-- image file is color, it will be converted to grayscale.
fromFileGray :: FilePath -> IO (HIplImage MonoChromatic Word8 NoROI)
fromFileGray fileName = 
  do checkFile fileName
     ptr <- cvLoadImage fileName LoadGray
     img <- fromPtr ptr :: IO (HIplImage MonoChromatic Word8 NoROI)
     addForeignPtrFinalizer cvFreePtr (imageDataOrigin img)
     return img

class LoadableFormat c d where
  loadFormat :: (c,d) -> FilePath -> IO (HIplImage c d NoROI)

instance LoadableFormat MonoChromatic Word8 where
  loadFormat _ = fromFileGray

instance LoadableFormat TriChromatic Word8 where
  loadFormat _ = fromFileColor

instance LoadableFormat MonoChromatic Word16 where
  loadFormat _ = fromPGM16

-- |An overloaded image file loader. The number of color channels and
-- color depth parts of the returned image's type must be inferrable
-- as they control how the image file is loaded.
fromFile :: forall c d. LoadableFormat c d => FilePath -> IO (HIplImage c d NoROI)
fromFile = loadFormat (undefined :: (c,d))

-- |Load a grayscale 'HIplImage' from a 16-bit image file. NOTE:
-- OpenCV (as of v2.2) does not correctly handle 16-bit PGM loading,
-- so this 16bpp loader is restricted to PGM. This loading routine
-- converts from Most Significant Byte first (MSB) byte ordering (as
-- per the PGM spec) to LSB byte ordering for x86 compatibility.
fromPGM16 :: FilePath -> IO (HIplImage MonoChromatic Word16 NoROI)
fromPGM16 fileName = 
  do checkFile fileName
     h <- openFile fileName ReadMode
     magic <- hGetLine h
     when (magic /= "P5") (hClose h >> 
                           error (fileName ++" is not a PGM"))
     (width, height) <- fmap ((read***read). second tail . break (==' '))
                             (hGetLine h)
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
     return $ Img 0 width height (fromIntegral numBytes) fp fp (2*width)

-- |Save an image to the specified file.
toFile :: (HasChannels c, HasDepth d) => FilePath -> HIplImage c d r -> IO ()
toFile fileName img = withHIplImage img $ \ptr -> cvSaveImage fileName ptr

-- |Allocate a new 'HIplImage' with the same dimensions, number of
-- color channels, and color depth as an existing HIplImage. The pixel
-- data of the original 'HIplImage' is not copied.
compatibleImage :: (HasChannels c, HasDepth d, ImgBuilder r) => 
                   HIplImage c d r -> IO (HIplImage c d r)
compatibleImage img = 
    do ptr <- mallocForeignPtrArray (fromIntegral (imageSize img))
       return $ buildImg 0 (width img) (height img) (roi img) 
                         (imageSize img) ptr ptr (widthStep img)

-- |Allocate a new 'IplImage' with the same dimensions, number of
-- color channels, and color depth as an existing 'HIplImage'. The
-- pixel data of the original 'HIplImage' is not copied.
compatibleImagePtr :: forall c d r. (HasChannels c, HasDepth d) =>
                      HIplImage c d r -> IO (ForeignPtr IplImage)
compatibleImagePtr img = createImageF (CvSize w' h') nc d
    where w' = fromIntegral . width $ img
          h' = fromIntegral . height $ img
          nc = fromIntegral . numChannels $ (undefined::c)
          d = depth (undefined::d)

compatibleImagePtrPtr :: Ptr IplImage -> IO (Ptr IplImage)
compatibleImagePtrPtr = 
    join . (liftA3 cvCreateImage <$> cvGetSize <*> getNumChannels <*> getDepth)

-- |Create an exact duplicate of the given HIplImage. This allocates a
-- fresh array to store the copied pixels.
duplicateImage :: (HasChannels c, HasDepth d, ImgBuilder r) =>
                  HIplImage c d r -> IO (HIplImage c d r)
--duplicateImage (HIplImage _ w h r sz pixels _ stride) =
duplicateImage img =
    do fptr <- mallocForeignPtrArray sz'
       withForeignPtr (imageData img) $ 
           \src -> withForeignPtr fptr $ \dst -> copyBytes dst src sz'
       return $ buildImg 0 (width img) (height img) (roi img) 
                         (imageSize img) fptr fptr (widthStep img)
  where sz' = fromIntegral (imageSize img)

-- |Clone an 'HIplImage', returning the 'Ptr' 'IplImage' underlying
-- the clone.
duplicateImagePtr :: (HasChannels c, HasDepth d) =>
                     HIplImage c d r -> IO (ForeignPtr IplImage)
duplicateImagePtr = flip withHIplImage cloneImageF

-- |Pass the given function a 'HIplImage' constructed from a width, a
-- height, and a 'V.Vector' of pixel values. The new 'HIplImage' \'s
-- pixel data is shared with the supplied 'V.Vector'.
withPixelVector :: forall a c d r. 
                   (HasChannels c, Integral a, HasDepth d) =>
                   a -> a -> V.Vector d -> (HIplImage c d NoROI -> r) -> r
withPixelVector w h pix f = if fromIntegral len == sz
                            then f $ Img 0 w' h' sz fp fp (w'*nc)
                            else error "Length disagreement"
    where w' = fromIntegral w
          h' = fromIntegral h
          nc = numChannels (undefined::c)
          sz = w' * h' * nc
          (fp,len) = case V.unsafeToForeignPtr (V.force pix) of
                         (fp,0,len) -> (fp,len)
                         _ -> error "fromPixels non-zero offset"

-- |Construct a fresh 'HIplImage' from a width, a height, and a
-- 'V.Vector' of pixel values.
fromPixels :: forall a c d. 
              (Integral a, HasChannels c, HasDepth d) =>
              a -> a -> V.Vector d -> HIplImage c d NoROI
fromPixels w h pix = unsafePerformIO $ 
                     do fp <- copyData
                        return $ Img 0 w' h' sz fp fp (w'*nc)
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
{-# INLINE [0] fromPixels #-}

-- |Helper function to explicitly type a vector of monochromatic pixel
-- data. Parameters are the output image's width, height, and pixel
-- content.
fromGrayPixels :: (HasDepth d, Integral a) => 
                  a -> a -> V.Vector d -> HIplImage MonoChromatic d NoROI
fromGrayPixels w h = isMono . fromPixels w h

-- |Helper function to explicitly type a vector of interleaved
-- trichromatic pixel data. Parameters are the output image's width,
-- height, and pixel content.
fromColorPixels :: (HasDepth d, Integral a) =>
                   a -> a -> V.Vector d -> HIplImage TriChromatic d NoROI
fromColorPixels w h = isColor . fromPixels w h

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' that is an exact duplicate of the
-- given 'HIplImage'. Returns the duplicate 'HIplImage' after
-- performing the given action along with the result of that action.
withDuplicateImage :: (HasChannels c, HasDepth d, ImgBuilder r) => 
                      HIplImage c d r -> (Ptr IplImage -> IO b) -> 
                      IO (HIplImage c d r, b)
withDuplicateImage img1 f = do img2 <- duplicateImage img1
                               r <- withHIplImage img2 f
                               return (img2, r)

-- |Provides the supplied function with a 'Ptr' to the 'IplImage'
-- underlying a new 'HIplImage' of the same dimensions as the given
-- 'HIplImage'.
withCompatibleImage :: (HasChannels c, HasDepth d, ImgBuilder r) => 
                       HIplImage c d r -> (Ptr IplImage -> IO b) -> 
                       IO (HIplImage c d r, b)
withCompatibleImage img1 f = do img2 <- compatibleImage img1
                                r <- withHIplImage img2 f
                                return (img2, r)

unsafeWithHIplImage :: (HasChannels c, HasDepth d) =>
                       HIplImage c d r -> (Ptr IplImage -> a) -> a
unsafeWithHIplImage img f = unsafePerformIO $ withHIplImage img (return . f)

-- |Extract a rectangular region of interest from an image. Returns a
-- new image whose pixel data is copied from the given rectangle of
-- the source image. Parameters are the upper-left corner of the
-- rectangle in image coordinates, the (width,height) of the rectangle
-- in pixels, and the source 'HIplImage'.
getRect :: (HasChannels c, HasDepth d) =>
           (Int,Int) -> (Int,Int) -> HIplImage c d r -> IO (HIplImage c d NoROI)
getRect (rx,ry) (rw,rh) src = 
    do img <- mkHIplImage (fromIntegral rw) (fromIntegral rh)
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
