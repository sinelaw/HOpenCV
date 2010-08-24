{-# LANGUAGE ScopedTypeVariables #-}
-- |Functions for working with 'HIplImage's.
module AI.CV.OpenCV.Core.HIplUtils 
    (isColor, isMono, imgChannels, withPixels, pixels,
     fromPtr, fromFileColor, fromFileGray, toFile, 
     compatibleImage, duplicateImage, fromPixels, 
     withImagePixels, fromGrayPixels, fromColorPixels,
     withDuplicateImage, withCompatibleImage, 
     HIplImage, mkHIplImage, width, height, 
     withHIplImage, FreshImage, MonoChromatic, 
     TriChromatic, HasChannels, HasDepth(..), 
     ByteOrFloat) where
import AI.CV.OpenCV.Core.CxCore (IplImage)
import AI.CV.OpenCV.Core.HighGui (cvLoadImage, cvSaveImage, LoadColor(..))
import AI.CV.OpenCV.Core.HIplImage
import Control.Monad.ST (runST, unsafeIOToST)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.Directory (doesFileExist)
import Unsafe.Coerce

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

-- |Return the number of color channels a 'HIplImage' has as a runtime
-- value.
imgChannels :: forall a c d. HasChannels c => HIplImage a c d -> Int
imgChannels _ = numChannels (undefined::c)

-- |Apply the supplied function to a 'V.Vector' containing the pixels
-- that make up an 'HIplImage'. This does not copy the underlying
-- data.
withPixels :: forall a c d r. (HasDepth d, Storable d) => 
              HIplImage a c d -> (V.Vector d -> r) -> r
withPixels img f = f $ V.unsafeFromForeignPtr (imageData img) 0 n
    where n = imageSize img `div` bytesPerPixel (undefined::d)

doST :: IO a -> a
doST x = runST (unsafeIOToST x)

-- |Return a 'V.Vector' containing a copy of the pixels that make up a
-- 'HIplImage'.
pixels :: Storable d => HIplImage a c d -> V.Vector d
pixels img = doST $ do ptr <- mallocForeignPtrBytes len
                       withForeignPtr ptr $ \dst -> 
                         withForeignPtr (imageData img) $ \src -> 
                             copyBytes dst src len
                       return $ V.unsafeFromForeignPtr ptr 0 len
    where len = imageSize img

-- |Read a 'HIplImage' from a 'Ptr' 'IplImage'
fromPtr :: (HasChannels c, HasDepth d, Storable d) => 
           Ptr IplImage -> IO (HIplImage () c d)
fromPtr = peek . castPtr

-- Ensure that a file exists.
checkFile :: FilePath -> IO ()
checkFile f = do e <- doesFileExist f
                 if e then return () else error $ "Can't find "++f

-- |Load a color 'HIplImage' from an 8-bit image file. If the image
-- file is grayscale, it will be converted to color.
fromFileColor :: FilePath -> IO (HIplImage FreshImage TriChromatic Word8)
fromFileColor fileName = do checkFile fileName
                            ptr <- cvLoadImage fileName LoadColor
                            img <- fromPtr ptr :: IO (HIplImage () TriChromatic Word8)
                            return $ unsafeCoerce img

-- |Load a grayscale 'HIplImage' from an 8-bit image file. If the
-- image file is color, it will be converted to grayscale.
fromFileGray :: FilePath -> IO (HIplImage FreshImage MonoChromatic Word8)
fromFileGray fileName = do checkFile fileName
                           ptr <- cvLoadImage fileName LoadGray
                           img <- fromPtr ptr :: IO (HIplImage () MonoChromatic Word8)
                           return $ unsafeCoerce img

-- |Save a 'HIplImage' to the specified file.
toFile :: (HasChannels c, HasDepth d, Storable d) => 
          FilePath -> HIplImage a c d -> IO ()
toFile fileName img = withHIplImage img $ \ptr -> cvSaveImage fileName ptr


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

-- |Pass the given function a 'HIplImage' constructed from a width, a
-- height, and a 'V.Vector' of pixel values. The new 'HIplImage' \'s
-- pixel data is shared with the supplied 'V.Vector'.
withImagePixels :: forall a c d r. 
                   (HasChannels c, Integral a, HasDepth d, Storable d) =>
                   a -> a -> V.Vector d -> (HIplImage () c d -> r) -> r
withImagePixels w h pix f = if fromIntegral len == sz
                            then f $ HIplImage 0 w' h' sz fp (w'*nc)
                            else error "Length disagreement"
    where w' = fromIntegral w
          h' = fromIntegral h
          nc = numChannels (undefined::c)
          sz = w' * h' * nc
          (fp,len) = case V.unsafeToForeignPtr (V.force pix) of
                         (fp,0,len) -> (fp,len)
                         _ -> error "fromPixels non-zero offset"
{-# INLINE [0] withImagePixels #-}

-- |Construct a fresh 'HIplImage' from a width, a height, and a
-- 'V.Vector' of pixel values.
fromPixels :: forall a c d. 
              (Integral a, HasChannels c, HasDepth d, Storable d) =>
              a -> a -> V.Vector d -> HIplImage FreshImage c d
fromPixels w h pix = doST $ do fp <- copyData
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
{-# INLINE [0] fromPixels #-}

-- |Helper function to explicitly type a vector of monochromatic pixel
-- data. Parameters are the output image's width, height, and pixel
-- content.
fromGrayPixels :: (HasDepth d, Storable d, Integral a) => 
                  a -> a -> V.Vector d -> HIplImage FreshImage MonoChromatic d
fromGrayPixels w h = isMono . fromPixels w h

-- |Helper function to explicitly type a vector of trichromatic pixel
-- data. Parameters are the output image's width, height, and pixel
-- content.
fromColorPixels :: (HasDepth d, Storable d, Integral a) =>
                   a -> a -> V.Vector d -> HIplImage FreshImage TriChromatic d
fromColorPixels w h = isColor . fromPixels w h

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
{-# NOINLINE withDuplicateImage #-}

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
{-# NOINLINE withCompatibleImage #-}