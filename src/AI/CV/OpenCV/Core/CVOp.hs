{-# LANGUAGE ScopedTypeVariables #-}
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
module AI.CV.OpenCV.Core.CVOp (cv, cv2) where
import AI.CV.OpenCV.Core.CxCore (IplArrayType, CvArr)
import AI.CV.OpenCV.Core.HIplUtil
import Control.Monad ((>=>), void)
import Data.Monoid
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

-- |A CV operation is an IO function on a 'HIplImage'.
newtype CVOp c d = CVOp { op :: Ptr CvArr -> IO () }

-- |A wrapper for operations that mutate an array in-place. The input
-- to such operations must be duplicated before being passed to the
-- operation.
cv :: forall a c d e.
      (HasChannels c, HasDepth d, IplArrayType e) => 
      (Ptr e -> IO a) -> HIplImage c d -> HIplImage c d
cv = runCV . mkCVOp
    where mkCVOp :: (Ptr e -> IO a) -> CVOp c d
          mkCVOp f = CVOp (void . f. castPtr)
{-# INLINE cv #-}

instance Monoid (CVOp c d) where
  mempty = CVOp . const $ return ()
  CVOp f `mappend` CVOp g = CVOp (\x -> g x >> f x)
  {-# INLINE mappend #-}

withClone :: (HasChannels c, HasDepth d) =>
             (Ptr e -> IO a) -> HIplImage c d -> IO (HIplImage c d)
withClone f = duplicateImagePtr >=> flip withForeignPtr (\x -> f (castPtr x) >> 
                                                               fromPtr x)

-- |Run a 'CVOp'.
runCV :: (HasChannels c, HasDepth d) => 
         CVOp c d -> HIplImage c d -> HIplImage c d
runCV = (unsafePerformIO .) . withClone . op
{-# NOINLINE runCV #-}

-- Apply a binary function to the same argument twice.
dupArg :: (Ptr e -> Ptr e -> IO ()) -> Ptr e -> IO ()
dupArg f = \x -> f x x

-- |Wrapper for operations that want an argument /and/ a compatible
-- destination buffer, but don't need a clone of an input.
cv2 :: forall a c1 d1 c2 d2 e.
       (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, 
        IplArrayType e) => 
       (Ptr e -> Ptr e -> IO a) -> HIplImage c1 d1 -> HIplImage c2 d2
cv2 = runBinOp . mkBinOp
    where mkBinOp :: (Ptr e -> Ptr e -> IO a) -> BinOp (c1,d1) (c2,d2)
          mkBinOp f = BinOp (\x y -> void (f (castPtr x) (castPtr y)))
{-# INLINE cv2 #-}

bi2unary :: BinOp (c,d) (c,d) -> CVOp c d
bi2unary = CVOp . dupArg . binop

unary2bi :: CVOp c d -> BinOp (c,d) (c,d)
unary2bi = BinOp . const . op

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

newtype BinOp a b = 
    BinOp { binop :: Ptr CvArr -> Ptr CvArr -> IO () }

-- Compose 'BinOp's for in-place mutation when the types allow it.
cbop :: BinOp b b -> BinOp a b -> BinOp a b
cbop (BinOp f) (BinOp g) = BinOp $ \x y -> g x y >> f y y

withDst :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2, 
            IplArrayType e) => 
           (Ptr e -> Ptr e -> IO a) ->
           HIplImage c1 d1 -> IO (HIplImage c2 d2)
withDst f img = do img2 <- mkHIplImage (width img) (height img)
                   _ <- withHIplImage img2 go
                   return img2
    where go x = withHIplImage img (flip f (castPtr x) . castPtr)

runBinOp :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) => 
            BinOp (c1,d1) (c2,d2) -> HIplImage c1 d1 -> HIplImage c2 d2
runBinOp = (unsafePerformIO .) . withDst . binop
{-# NOINLINE runBinOp #-}

{-# RULES "runCV/fuse"
    forall f g x. runCV f (runCV g x) = runCV (f <> g) x #-}

{-# RULES "runBinOp/fuse"
    forall f g x. runBinOp f (runBinOp g x) = runBinOp (cbop f g) x #-}

{-# RULES "runCV/runBinOp/fuse"
    forall f g x. runCV f (runBinOp g x) = runBinOp (cbop (unary2bi f) g) x #-}

{-# RULES "runBinOp/runCV/fuse"
    forall f g x. runBinOp f (runCV g x) = runCV (bi2unary f <> g) x #-}
