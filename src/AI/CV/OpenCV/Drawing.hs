module AI.CV.OpenCV.Drawing where
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp
import Foreign.C.String
import Foreign.Ptr

putText :: (HasChannels c, HasDepth d) => 
           String -> (Int,Int) -> (Double,Double,Double) -> 
           HIplImage c d -> HIplImage c d
putText msg (x,y) (r,g,b) = cv $ \dst ->
                            withCString msg $ \msg' ->
                                c_cvPutText (castPtr dst) msg' (fi x) (fi y) 
                                            (fr r) (fr g) (fr b)
    where fi = fromIntegral
          fr = realToFrac
{-# INLINE putText #-}

-- |Type of line to draw.
data LineType = EightConn -- ^8-connected line
              | FourConn  -- ^4-connected line
              | AALine    -- ^antialiased line

-- |An RGB triple. 
type RGB = (Double, Double, Double)

-- |Convert a LineType into an integer.
lineTypeEnum :: LineType -> Int
lineTypeEnum EightConn = 8
lineTypeEnum FourConn  = 4
lineTypeEnum AALine    = 16

-- |Draw each line, defined by its endpoints, on a duplicate of the
-- given 'HIplImage' using the specified RGB color, line thickness,
-- and aliasing style.
drawLines :: (HasChannels c, HasDepth d) =>
             RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
             HIplImage c d -> HIplImage c d
drawLines col thick lineType lines = 
    cv $ \img -> mapM_ (draw img) lines
    where draw ptr (pt1, pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType
{-# INLINE drawLines #-}
