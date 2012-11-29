{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances,
             TypeSynonymInstances, CPP, DataKinds, KindSignatures, GADTs #-}
-- |Combinators that fuse compositions of image processing operations
-- for in-place mutation.
--
-- The 'cv' wrapper is intended for operations that take a single
-- array argument, and mutate that array in-place. A canonical example
-- is a function that draws lines on an image. Compositions of such
-- operations may all mutate the same image in-place, but an argument
-- given to the composition, or indeed given to a standalone operation
-- of this variety, must be duplicated before being operated upon.
--
-- In contrast, the 'cv2' wrapper is for operations that take separate
-- @src@ and @dst@ (source and destination) arguments. When the image
-- types of these arguments are the same, it is possible to supply the
-- same value for both arguments, thus avoiding an image allocation. A
-- standalone operation of this variety, or a composition beginning
-- with such an operation, must have a destination image
-- allocated. This is cheaper than duplicating the input image as with
-- operations wrapped by the `cv` combinator.
module OpenCV.Core.CVOp (cv, Inplace(..)) where
import OpenCV.Core.CxCore (IplArrayType, CvArr)
import OpenCV.Core.ImageUtil
import OpenCV.Core.Image
import Control.Monad (void)
import Data.Int
import Data.Monoid
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Data.Word (Word8, Word16)

-- |A CV operation is an IO function on a 'Image'.
newtype CVOp (c::Channels) d = CVOp { op :: Ptr CvArr -> IO () }

-- |A wrapper for operations that mutate an array in-place. The input
-- to such operations must be duplicated before being passed to the
-- operation.
cv :: forall a c d e r1 r2.
      (HasDepth d, IplArrayType e, UpdateROI r2) => 
      (Ptr e -> IO a) -> Image c d r1 -> Image c d r2
--cv = runCV . mkCVOp
cv f img@Image{} = runCV (mkCVOp f) img
    where mkCVOp :: (Ptr e -> IO a) -> CVOp c d
          mkCVOp f = CVOp (void . f. castPtr)
{-# INLINE cv #-}

instance Monoid (CVOp c d) where
  mempty = CVOp . const $ return ()
  CVOp f `mappend` CVOp g = CVOp (\x -> g x >> f x)
  {-# INLINE mappend #-}

withClone :: UpdateROI r2 => 
             (Ptr e -> IO a) -> Image c d r1 -> IO (Image c d r2)
withClone f img@Image{} = duplicateImagePtr img >>= 
                          flip withForeignPtr (\x -> f (castPtr x) >> peekIpl x)

-- |Run a 'CVOp'.
runCV :: UpdateROI r2 => CVOp c d -> Image c d r1 -> Image c d r2
runCV = (unsafeDupablePerformIO .) . withClone . op
{-# NOINLINE runCV #-}

-- Apply a binary function to the same argument twice.
dupArg :: (Ptr e -> Ptr e -> IO a) -> Ptr e -> IO a
dupArg f = \x -> f x x

-- |Wrapper for operations that want an argument /and/ a compatible
-- destination buffer, but don't need a clone of an input.
cv2Alloc :: forall a c1 d1 c2 d2 e r. (HasDepth d1, HasDepth d2, SingI c2) =>
            (Ptr e -> Ptr e -> IO a) -> Image c1 d1 r -> Image c2 d2 r
cv2Alloc = runBinOp . mkBinOp
    where mkBinOp :: (Ptr e -> Ptr e -> IO a) -> BinOp c1 d1 c2 d2
          mkBinOp f = BinOp (\x y -> void (f (castPtr x) (castPtr y)))
{-# INLINE cv2Alloc #-}

bi2unary :: BinOp c d c d -> CVOp c d
bi2unary = CVOp . dupArg . binop

unary2bi :: CVOp c d -> BinOp c d c d
unary2bi = BinOp . const . op

-- |Some operations benefit from operating in-place over a defined
-- region-of-interest (ROI). If an operation must recompute every
-- pixel of the result image, then there is no need to initialize a
-- fresh destination image to the contents of the source
-- image. However, if we will only be recomputing a ROI of the source
-- image, then the result image should be initialized as a copy of the
-- source image so that its contents outside the ROI match the
-- original image.
class (HasDepth d1, HasDepth d2, SingI c2) => 
      Inplace (r::ROIEnabled) (c1::Channels) d1 (c2::Channels) d2 where
  cv2 :: IplArrayType e =>
           (Ptr e -> Ptr e -> IO a) -> Image c1 d1 r -> Image c2 d2 r
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

-- | We clone an image and operate within its ROI if the source and
-- destination images are compatible (same number of channels and
-- pixel depth).
instance (c1~c2, HasDepth d, SingI c2) => Inplace HasROI c1 d c2 d where
  cv2 = cv . dupArg
  {-# INLINE [1] cv2 #-}

instance (HasDepth d1, HasDepth d2) => 
         Inplace HasROI Trichromatic d1 Monochromatic d2 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance (HasDepth d1, HasDepth d2) => 
         Inplace HasROI Monochromatic d1 Trichromatic d2 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance SingI c2 => Inplace HasROI c1 Word8 c2 Float where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance SingI c2 => Inplace HasROI c1 Word8 c2 Word16 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance SingI c2 => Inplace HasROI c1 Word8 c2 Double where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance SingI c2 => Inplace HasROI c1 Word16 c2 Word8 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance SingI c2 => Inplace HasROI c1 Float c2 Word8 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

instance (HasDepth d1, HasDepth d2, SingI c2) => Inplace NoROI c1 d1 c2 d2 where
  cv2 = cv2Alloc
  {-# INLINE [1] cv2 #-}

-- If the source and destination are not compatible, then it doesn't
-- matter if there is a ROI set as we can never operate in-place.
instance SingI c2 => Inplace HasROI c1 Word8 c2 Int16 where

instance SingI c2 => Inplace HasROI c1 Float c2 Word16 where

instance SingI c2 => Inplace HasROI c1 Float c2 Int16 where

instance SingI c2 => Inplace HasROI c1 Float c2 Double where

instance SingI c2 => Inplace HasROI c1 Word16 c2 Int16 where

instance SingI c2 => Inplace HasROI c1 Word16 c2 Float where

instance SingI c2 => Inplace HasROI c1 Word16 c2 Double where

instance SingI c2 => Inplace HasROI c1 Double c2 Float where

instance SingI c2 => Inplace HasROI c1 Double c2 Word16 where

instance SingI c2 => Inplace HasROI c1 Double c2 Word8 where

instance SingI c2 => Inplace HasROI c1 Double c2 Int16 where

instance SingI c2 => Inplace HasROI c1 Int16 c2 Word8 where

instance SingI c2 => Inplace HasROI c1 Int16 c2 Word16 where

-- |This can be in-place due to the common representation.
instance SingI c2 => Inplace HasROI c1 Int16 c2 Float where

newtype BinOp (c1::Channels) d1 (c2::Channels) d2 = 
    BinOp { binop :: Ptr CvArr -> Ptr CvArr -> IO () }

-- Compose 'BinOp's for in-place mutation when the types allow it.
cbop :: BinOp c d c d -> BinOp c0 d0 c d -> BinOp c0 d0 c d
cbop (BinOp f) (BinOp g) = BinOp $ \x y -> g x y >> f y y

withDst :: (HasDepth d1, HasDepth d2, IplArrayType e, SingI c2) => 
           (Ptr e -> Ptr e -> IO a) ->
           Image c1 d1 r -> IO (Image c2 d2 r)
withDst f img = do img2' <- mallocImage (width img) (height img)
                   let img2 = updateROI (roi img) img2'
                   _ <- withIplImage img2 go
                   return img2
    where go x = withIplImage img (flip f (castPtr x) . castPtr)

runBinOp :: (HasDepth d1, HasDepth d2, SingI c2) => 
            BinOp c1 d1 c2 d2 -> Image c1 d1 r -> Image c2 d2 r
runBinOp = (unsafeDupablePerformIO .) . withDst . binop
{-# NOINLINE runBinOp #-}

{-# RULES "runCV/fuse"
    forall f g x. runCV f (runCV g x) = runCV (f <> g) x #-}

{-# RULES "runBinOp/fuse"
    forall f g x. runBinOp f (runBinOp g x) = runBinOp (cbop f g) x #-}

{-# RULES "runCV/runBinOp/fuse"
    forall f g x. runCV f (runBinOp g x) = runBinOp (cbop (unary2bi f) g) x #-}

{-# RULES "runBinOp/runCV/fuse"
    forall f g x. runBinOp f (runCV g x) = runCV (bi2unary f <> g) x #-}
