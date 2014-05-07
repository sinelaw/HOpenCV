{-# LANGUAGE ForeignFunctionInterface #-}
module OpenCV.Drawing (prepFont, prepFontAlt, putText, FontFace(..), 
                       LineType(..), RGB, drawLines, fillConvexPoly) where
import OpenCV.Core.CxCore
import OpenCV.Core.ImageUtil
import OpenCV.Core.CVOp
import OpenCV.Core.StorableUtil
import Data.Bits ((.|.))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (withArray)
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
    where face' | italic = fi (fromEnum face) .|. italicFont
                | otherwise = fi $ fromEnum face
          lt = fi $ lineTypeEnum ltype
          fi = fromIntegral

-- |Default sans-serif font.
defaultFont :: Ptr CvFont
defaultFont = unsafePerformIO $ initFont NormalSans False 1 1 0 1 EightConn
{-# NOINLINE defaultFont #-}

-- |Produce a text-drawing function given a font description. The
-- application @prepFont face italic hscale vscale thickness@ produces
-- a text-drawing function using a font with the given @face@ (which
-- may be @italic@), horizontal and verticale scale, and line
-- @thickness@.
prepFont :: (HasDepth d, UpdateROI r) =>
            FontFace -> Bool -> CDouble -> CDouble -> CInt -> 
            IO ((CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
                Image c d r -> Image c d r)
prepFont face italic hscale vscale thickness = 
    prepFontAlt face italic hscale vscale 0 thickness EightConn
{-# INLINE prepFont #-}

#include <opencv2/core/core_c.h>

#def void cvPutText_wrap(CvArr* img, const char* text, CvPoint* org,\
                         const CvFont* font, CvScalar* color) {\
  cvPutText(img, text, *org, font, *color);\
}

foreign import ccall "cvPutText_wrap" 
  c_cvPutText :: Ptr CvArr -> CString -> Ptr CvPoint -> 
                 Ptr CvFont -> Ptr CvScalar -> IO ()

-- |Produce a text-drawing function given a font description. The
-- application @prepFontAlt face italic hscale vscale shear thickness
-- ltype@ produces a text-drawing function using a font with the given
-- @face@ (which may be @italic@), horizontal and vertical scale,
-- @shear@, line @thickness@, and line type.
prepFontAlt :: (HasDepth d, UpdateROI r) =>
               FontFace -> Bool -> CDouble -> CDouble -> CDouble -> 
               CInt -> LineType ->
               IO ((CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
                   Image c d r -> Image c d r)
prepFontAlt face italic hscale vscale shear thickness ltype =     
    do f <- initFont face italic hscale vscale shear thickness ltype
       let go (x,y) (r,g,b) msg = cv $ \dst -> 
                                  withCString msg $ \msg' ->
                                  withS (CvPoint x y) $ \ptPtr ->
                                  withS (CvScalar r g b 1) $ \colPtr ->
                                    c_cvPutText dst msg' ptPtr f colPtr
                                      --cvPutText dst msg' x y f r g b
           {-# INLINE go #-}
       return go
{-# INLINE prepFontAlt #-}

putText :: (HasDepth d, UpdateROI r) => 
           (CInt, CInt) -> (CDouble, CDouble, CDouble) -> String -> 
           Image c d r -> Image c d r
putText (x,y) (r,g,b) msg = cv $ \dst ->
                            withCString msg $ \msg' ->
                            withS (CvPoint x y) $ \ptPtr ->
                            withS (CvScalar r g b 1) $ \colPtr ->
                              c_cvPutText dst msg' ptPtr defaultFont colPtr
                                -- cvPutText (castPtr dst) msg' (fi x) (fi y) 
                                --            (fr r) (fr g) (fr b)
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
-- given 'Image' using the specified RGB color, line thickness,
-- and aliasing style.
drawLines :: (HasDepth d, UpdateROI r) =>
             RGB -> Int -> LineType -> [((Int,Int),(Int,Int))] -> 
             Image c d r -> Image c d r
drawLines col thick lineType lines = 
    cv $ \img -> mapM_ (draw img) lines
    where draw ptr (pt1, pt2) = cvLine ptr pt1 pt2 col thick lineType'
          lineType' = lineTypeEnum lineType
{-# INLINE drawLines #-}

-- |Draw a filled, convex polygon. Can draw all monotonic polygons
-- without self-intersections including those with horizontal top or
-- bottom edges.
fillConvexPoly :: (HasDepth d, UpdateROI r) =>
                  RGB -> LineType -> [(Int,Int)] -> Image c d r -> Image c d r
fillConvexPoly (r,g,b) lineType pts = 
  cv $ \img -> withArray (concatMap flatten pts) $ \pts' ->
               c_cvFillConvexPoly img pts' (fi $ length pts) 
                                  (fr r) (fr g) (fr b) 0 lt 0
  where lt = fi $ lineTypeEnum lineType
        flatten (x,y) = [fi x, fi y]
        fi = fromIntegral
        fr = realToFrac



