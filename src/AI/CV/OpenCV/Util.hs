module AI.CV.OpenCV.Util
where

import Foreign

withForeignPtr2 :: ForeignPtr a -> ForeignPtr b
                   -> (Ptr a -> Ptr b -> IO c)
                   -> IO c
withForeignPtr2 a b f
  = let g a' = withForeignPtr b $ f a'
    in withForeignPtr a g
