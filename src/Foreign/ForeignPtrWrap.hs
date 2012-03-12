module Foreign.ForeignPtrWrap where

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Error

-- | A wrapper for newForeignPtr that handles nullPtrs, and can be chained to an IO Ptr creator.
--
-- Example usage:
--
-- > myPtrCreator = (createForeignPtr deallocFunc) . allocFunc
--
-- where, allocFunc :: a->b->c->...-> IO (Ptr z)
createForeignPtr :: (FunPtr (Ptr a -> IO () )) -> IO (Ptr a) -> IO (ForeignPtr a)
createForeignPtr dealloc allocedPtr = do
    ptr <- checkPtr allocedPtr
    newForeignPtr dealloc ptr

-- | Fails if the ptr is nullPtr
checkPtr :: IO (Ptr a) -> IO (Ptr a)
checkPtr x = do 
  res <- x
  if res /= nullPtr 
    then return res 
    else fail "Null Pointer"

------------------------------------------------
-- | Names a failure
errorName :: String -> IO a -> IO a
errorName = modifyIOError . const . userError
