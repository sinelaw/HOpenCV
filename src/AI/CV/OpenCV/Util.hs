module AI.CV.OpenCV.Util
where

fromToInteger :: (Integral a, Num b) => a -> b
fromToInteger = fromInteger . toInteger
