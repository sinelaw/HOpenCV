-- |A convenient 8-bit-per-channel RGB data type with a 'Storable'
-- instance.
module OpenCV.Color where
import Control.Applicative
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

data RGB8 = RGB8 {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Word8

instance Storable RGB8 where
  sizeOf _ = 3
  alignment _ = 4
  peek ptr = RGB8 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where ptr' = castPtr ptr
  poke ptr (RGB8 r g b) = do poke ptr' r
                             pokeElemOff ptr' 1 g
                             pokeElemOff ptr' 2 b
    where ptr' = castPtr ptr

-- |Map a function over each component of an RGB triple.
rgbmap :: (Word8 -> Word8) -> RGB8 -> RGB8
rgbmap f (RGB8 r g b) = RGB8 (f r) (f g) (f b)
