{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module AI.CV.OpenCV.Drawing (prepFont, prepFontAlt, putText, FontFace(..), 
                             LineType(..), RGB, drawLines) where
import AI.CV.OpenCV.Core.CxCore
import AI.CV.OpenCV.Core.HIplUtil
import AI.CV.OpenCV.Core.CVOp
import Data.Bits ((.|.))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

-- |The available font faces are a subset of the Hershey fonts.
data FontFace = NormalSans 
              | SmallSans 
              | ComplexSans 
              | NormalSerif
              | ComplexSerif
              | SmallSerif
              | Script 
              | ComplexScript
                deriving Enum

initFont :: FontFace -> Bool -> CDouble -> CDouble -> CDouble -> CInt -> 
            LineType -> IO (Ptr CvFont)
initFont face italic hscale vscale shear thickness ltype = 
    do p <- malloc
       cvInitFont p face' hscale vscale shear thickness lt
       return p
    where face' | italic = (fi $ fromEnum face) .|. italicFont
                | otherwise = fi $ fromEnum face
          lt = fi $ lineTypeEnum ltype
          fi = fromIntegral

-- |Default sans-serif font.
defaultFont :: Ptr CvFont
defaultFont = unsafePerformIO $ initFont NormalSans False 1 1 0 1 EightConn

-- |Produce a text-drawing function given a font description. The
-- application @prepFont face italic hscale vscale thickness@ produces
-- a text-drawing function using a font with the given @face@ (which
-- may be @italic@), horizontal and verticale scale, and line
-- @thickness@.
prepFont :: (HasChannels c, HasDepth d) =>
            FontFace -> Bool -> CDouble -> CDouble -> CInt -> 
            IO ((CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
                HIplImage c d -> HIplImage c d)
prepFont face italic hscale vscale thickness = 
    prepFontAlt face italic hscale vscale 0 thickness EightConn
{-# INLINE prepFont #-}

-- |Produce a text-drawing function given a font description. The
-- application @prepFontAlt face italic hscale vscale shear thickness
-- ltype@ produces a text-drawing function using a font with the given
-- @face@ (which may be @italic@), horizontal and vertical scale,
-- @shear@, line @thickness@, and line type.
prepFontAlt :: (HasChannels c, HasDepth d) =>
               FontFace -> Bool -> CDouble -> CDouble -> CDouble -> 
               CInt -> LineType ->
               IO ((CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
                   HIplImage c d -> HIplImage c d)
prepFontAlt face italic hscale vscale shear thickness ltype =     
    do f <- initFont face italic hscale vscale shear thickness ltype
       let go (x,y) (r,g,b) msg = cv $ \dst -> 
                                  withCString msg $ \msg' ->
                                      cvPutText dst msg' x y f r g b
           {-# INLINE go #-}
       return $ go
{-# INLINE prepFontAlt #-}

putText :: (HasChannels c, HasDepth d) => 
           (CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
           HIplImage c d -> HIplImage c d
putText (x,y) (r,g,b) msg = cv $ \dst ->
                            withCString msg $ \msg' ->
                                cvPutText dst msg' x y defaultFont r g b
                                -- c_cvPutText (castPtr dst) msg' (fi x) (fi y) 
                                --             (fr r) (fr g) (fr b)
    -- where fi = fromIntegral
    --       fr = realToFrac
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
