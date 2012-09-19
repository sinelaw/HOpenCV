{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances,
             TypeSynonymInstances, CPP #-}
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
import OpenCV.Core.HIplUtil
import OpenCV.Core.HIplImage
import Control.Monad ((>=>), void)
import Data.Int
import Data.Monoid
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe
import Data.Word (Word8, Word16)

-- |A CV operation is an IO function on a 'HIplImage'.
newtype CVOp c d = CVOp { op :: Ptr CvArr -> IO () }

-- |A wrapper for operations that mutate an array in-place. The input
-- to such operations must be duplicated before being passed to the
-- operation.
cv :: forall a c d e r1 r2.
      (HasChannels c, HasDepth d, ImgBuilder r1, ImgBuilder r2, IplArrayType e) => 
      (Ptr e -> IO a) -> HIplImage c d r1 -> HIplImage c d r2
cv = runCV . mkCVOp
    where mkCVOp :: (Ptr e -> IO a) -> CVOp c d
          mkCVOp f = CVOp (void . f. castPtr)
{-# INLINE cv #-}

instance Monoid (CVOp c d) where
  mempty = CVOp . const $ return ()
  CVOp f `mappend` CVOp g = CVOp (\x -> g x >> f x)
  {-# INLINE mappend #-}

withClone :: (HasChannels c, HasDepth d, ImgBuilder r1, ImgBuilder r2) =>
             (Ptr e -> IO a) -> HIplImage c d r1 -> IO (HIplImage c d r2)
withClone f = duplicateImagePtr >=> flip withForeignPtr (\x -> f (castPtr x) >> 
                                                               fromPtr x)

-- |Run a 'CVOp'.
runCV :: (HasChannels c, HasDepth d, ImgBuilder r1, ImgBuilder r2) => 
         CVOp c d -> HIplImage c d r1 -> HIplImage c d r2
runCV = (unsafeDupablePerformIO .) . withClone . op
{-# NOINLINE runCV #-}

-- Apply a binary function to the same argument twice.
dupArg :: (Ptr e -> Ptr e -> IO a) -> Ptr e -> IO a
dupArg f = \x -> f x x

-- |Wrapper for operations that want an argument /and/ a compatible
-- destination buffer, but don't need a clone of an input.
cv2Alloc :: forall a c1 d1 c2 d2 e r.
            (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, 
             IplArrayType e, ImgBuilder r) => 
            (Ptr e -> Ptr e -> IO a) -> HIplImage c1 d1 r -> HIplImage c2 d2 r
cv2Alloc = runBinOp . mkBinOp
    where mkBinOp :: (Ptr e -> Ptr e -> IO a) -> BinOp (c1,d1) (c2,d2)
          mkBinOp f = BinOp (\x y -> void (f (castPtr x) (castPtr y)))
{-# INLINE cv2Alloc #-}

bi2unary :: BinOp (c,d) (c,d) -> CVOp c d
bi2unary = CVOp . dupArg . binop

unary2bi :: CVOp c d -> BinOp (c,d) (c,d)
unary2bi = BinOp . const . op

#ifdef MAX_VERSION_base(4,4,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- |Some operations benefit from operating in-place over a defined
-- region-of-interest (ROI). If an operation must recompute every
-- pixel of the result image, then there is no need to initialize a
-- fresh destination image to the contents of the source
-- image. However, if we will only be recomputing a ROI of the source
-- image, then the result image should be initialized as a copy of the
-- source image so that its contents outside the ROI match the
-- original image.
class (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, ImgBuilder r) =>
      Inplace r c1 d1 c2 d2 where
  cv2 :: IplArrayType e =>
           (Ptr e -> Ptr e -> IO a) -> HIplImage c1 d1 r -> HIplImage c2 d2 r
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

-- | We clone an image and operate within its ROI if the source and
-- destination images are compatible (same number of channels and
-- pixel depth).
instance (HasChannels c, HasDepth d) => Inplace HasROI c d c d where
  cv2 = cv . dupArg
  {-# INLINE cv2 #-}

instance (HasDepth d1, HasDepth d2) => 
         Inplace HasROI Trichromatic d1 Monochromatic d2 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasDepth d1, HasDepth d2) => 
         Inplace HasROI Monochromatic d1 Trichromatic d2 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word8 c2 Float where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word8 c2 Word16 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word8 c2 Double where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word16 c2 Word8 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Float c2 Word8 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

instance (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) =>
         Inplace NoROI c1 d1 c2 d2 where
  cv2 = cv2Alloc
  {-# INLINE cv2 #-}

-- If the source and destination are not compatible, then it doesn't
-- matter if there is a ROI set as we can never operate in-place.
instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word8 c2 Int16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Float c2 Word16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Float c2 Int16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Float c2 Double where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word16 c2 Int16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word16 c2 Float where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Word16 c2 Double where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Double c2 Float where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Double c2 Word16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Double c2 Word8 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Double c2 Int16 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Int16 c2 Word8 where

instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Int16 c2 Word16 where

-- |This can be in-place due to the common representation.
instance (HasChannels c1, HasChannels c2) => 
         Inplace HasROI c1 Int16 c2 Float where

newtype BinOp a b = 
    BinOp { binop :: Ptr CvArr -> Ptr CvArr -> IO () }

-- Compose 'BinOp's for in-place mutation when the types allow it.
cbop :: BinOp b b -> BinOp a b -> BinOp a b
cbop (BinOp f) (BinOp g) = BinOp $ \x y -> g x y >> f y y

withDst :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, 
            ImgBuilder r, IplArrayType e) => 
           (Ptr e -> Ptr e -> IO a) ->
           HIplImage c1 d1 r -> IO (HIplImage c2 d2 r)
withDst f img = do img2' <- mkHIplImage (width img) (height img)
                   let img2 = addMaybeROI (roi img) img2'
                   _ <- withHIplImage img2 go
                   return img2
    where go x = withHIplImage img (flip f (castPtr x) . castPtr)

runBinOp :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, ImgBuilder r) => 
            BinOp (c1,d1) (c2,d2) -> HIplImage c1 d1 r -> HIplImage c2 d2 r
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
