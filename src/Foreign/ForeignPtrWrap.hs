module Foreign.ForeignPtrWrap where

import Foreign.Ptr
import Foreign.ForeignPtr

createForeignPtr :: (a -> IO (Ptr b)) -> (FunPtr (Ptr b -> IO () )) -> a -> IO (Maybe (ForeignPtr b) )
createForeignPtr alloc dealloc x = do
    ptr <- alloc x
    if ptr /= nullPtr
        then do
           foreignPtr <- newForeignPtr dealloc ptr
           return $ Just foreignPtr
        else
           return Nothing


