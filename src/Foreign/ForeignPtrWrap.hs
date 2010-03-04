module Foreign.ForeignPtrWrap where

import Foreign.Ptr
import Foreign.ForeignPtr

-- A wrapper for newForeignPtr that handles nullPtrs, and can be chained to an IO Ptr creator.
-- usage:
-- myPtrCreator = (createForeignPtr deallocFunc) . allocFunc
-- where, allocFunc :: a->b->c->...-> IO (Ptr z)
createForeignPtr :: (FunPtr (Ptr a -> IO () )) -> IO (Ptr a) -> IO (Maybe (ForeignPtr a))
createForeignPtr dealloc allocedPtr = do
    ptr <- allocedPtr
    if ptr /= nullPtr
        then do
           foreignPtr <- newForeignPtr dealloc ptr
           return $ Just foreignPtr
        else
           return Nothing


