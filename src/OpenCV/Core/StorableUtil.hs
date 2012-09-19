module OpenCV.Core.StorableUtil where
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (Ptr)

withS :: Storable a => a -> (Ptr a -> IO b) -> IO b
withS x f = alloca $ \ptr -> poke ptr x >> f ptr