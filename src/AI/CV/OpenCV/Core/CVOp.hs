module AI.CV.OpenCV.Core.CVOp (cv, cv2) where
import AI.CV.OpenCV.Core.CxCore (IplImage)
import AI.CV.OpenCV.Core.HIplUtil
import Control.Monad ((>=>), void)
import Data.Monoid
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe

import Control.Category (Category)
import qualified Control.Category as Cat


-- |A CV operation is an IO function on a 'HIplImage'.
newtype CVOp c d = CVOp { op :: Ptr IplImage -> IO () }

cv :: (HasChannels c, HasDepth d) => 
      (Ptr IplImage -> IO a) -> HIplImage c d -> HIplImage c d
cv = runCV . CVOp . (void .)
{-# INLINE cv #-}

instance Monoid (CVOp c d) where
  mempty = CVOp . const $ return ()
  CVOp f `mappend` CVOp g = CVOp (\x -> g x >> f x)
  {-# INLINE mappend #-}

withClone :: (HasChannels c, HasDepth d) =>
             (Ptr IplImage -> IO a) -> HIplImage c d -> IO (HIplImage c d)
withClone f = duplicateImagePtr >=> flip withForeignPtr (\x -> f x >> fromPtr x)

-- |Run a 'CVOp'.
-- runCV :: (HasChannels c, HasDepth d) => CVOp r -> HIplImage c d -> HIplImage c d
-- runCV = ((unsafePerformIO . fmap fst) .) . flip withDuplicateImageIO . runKleisli
runCV :: (HasChannels c, HasDepth d) => 
         CVOp c d -> HIplImage c d -> HIplImage c d
runCV = (unsafePerformIO .) . withClone . op
{-# NOINLINE runCV #-}

-- makeUnary f = \img -> do dst <- compatibleImagePtrPtr img
--                          f img dst
--                          return dst

newtype CVOpBi c d = CVOpBi { opbi :: Ptr IplImage -> Ptr IplImage -> IO () }

instance Monoid (CVOpBi c d) where
    mempty = CVOpBi $ \ _ _ -> return ()
    CVOpBi f `mappend` CVOpBi g = CVOpBi $ \x y -> g x y >> f y y
    {-# INLINE mappend #-}

{-
withComp :: (HasChannels c, HasDepth d) =>
            (Ptr IplImage -> Ptr IplImage -> IO a) -> 
            HIplImage c d -> IO (HIplImage c d)
withComp f img = compatibleImagePtr img >>= 
                 flip withForeignPtr (\x -> withHIplImage img (flip f x) >> 
                                            fromPtr x)
-}
withComp :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) =>
            (Ptr IplImage -> Ptr IplImage -> IO a) -> 
            HIplImage c1 d1 -> IO (HIplImage c2 d2)
withComp f img = mkHIplImage (width img) (height img) >>= \img2 -> 
                 withHIplImage img2 (\x -> withHIplImage img (flip f x) >> 
                                           return img2)


runCVComp :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) => 
             CVOpBi c d -> HIplImage c1 d1 -> HIplImage c2 d2
runCVComp = (unsafePerformIO .) . withComp . opbi
{-# NOINLINE runCVComp #-}

-- Apply a binary function to the same argument twice.
dupArg :: (Ptr IplImage -> Ptr IplImage -> IO ()) -> Ptr IplImage -> IO ()
dupArg f = \x -> f x x

-- |Operations that want an argument /and/ a compatible destination
-- buffer, but don't need a clone of an input.
cv2 :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) => 
       (Ptr IplImage -> Ptr IplImage -> IO a) -> 
       HIplImage c1 d1 -> HIplImage c2 d2
--cv2 = runCVComp . CVOpBi . ((void .) .)
cv2 = runBinOp . BinOp . ((void .) .)
{-# INLINE cv2 #-}
{-
bi2unary :: CVOpBi c d -> CVOp c d
bi2unary = CVOp . dupArg . opbi

unary2bi :: CVOp c d -> CVOpBi c d
unary2bi = CVOpBi . const . op
-}

bi2unary :: BinOp (c,d) (c,d) -> CVOp c d
bi2unary = CVOp . dupArg . binop

unary2bi :: CVOp c d -> BinOp (c,d) (c,d)
unary2bi = BinOp . const . op

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

newtype BinOp a b = 
    BinOp { binop :: Ptr IplImage -> Ptr IplImage -> IO () }

instance Category BinOp where
    id = BinOp . const . const $ return ()
    BinOp f . BinOp g = BinOp $ \x y -> g x y >> f y y

withDst :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) => 
           (Ptr IplImage -> Ptr IplImage -> IO a) ->
           HIplImage c1 d1 -> IO (HIplImage c2 d2)
withDst f img = mkHIplImage (width img) (height img) >>= \img2 ->
                withHIplImage img2 (\x -> withHIplImage img (flip f x) >>
                                          return img2)

runBinOp :: (HasChannels c1, HasDepth d1, HasChannels c2, HasDepth d2) => 
            BinOp (c1,d1) (c2,d2) -> HIplImage c1 d1 -> HIplImage c2 d2
runBinOp = (unsafePerformIO .) . withDst . binop

{-# RULES "runCV/fuse"
    forall f g x. runCV f (runCV g x) = runCV (f <> g) x #-}

{-# RULES "runBinOp/fuse"
    forall f g x. runBinOp f (runBinOp g x) = runBinOp ((Cat..) f g) x #-}

{-# RULES "runCV/runBinOp/fuse"
    forall f g x. runCV f (runBinOp g x) = runBinOp ((Cat..) (unary2bi f) g) x #-}

{-# RULES "runBinOp/runCV/fuse"
    forall f g x. runBinOp f (runCV g x) = runCV (bi2unary f <> g) x #-}


{-# RULES "cvComp/cvComp/fuse"
    forall f g x. runCVComp f (runCVComp g x) = runCVComp (f <> g) x #-}

{- RULES "runCV/cvComp/apply"
    forall f g x. runCV f (runCVComp g x) = runCVComp (unary2bi f <> g) x -}

{- RULES "cvComp/runCV/apply"
    forall f g x. runCVComp f (runCV g x) = runCV (bi2unary f <> g) x -}