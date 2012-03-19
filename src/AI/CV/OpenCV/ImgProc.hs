{-# Language ForeignFunctionInterface #-}

module AI.CV.OpenCV.ImgProc
where

import Foreign
import Foreign.C
import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.Util

foreign import ccall unsafe "imgproc.h cvCvtColor"
  cvCvtColor :: Ptr Priv_CvArr -> Ptr Priv_CvArr -> CInt -> IO ()

cvtColor :: (IplArrayType a, IplArrayType b) => a -> b -> CvtColorFlag -> IO ()
cvtColor i j f
  = do CvArr i' <- fromArr i
       CvArr j' <- fromArr j
       withForeignPtr2 i' j'
        $ \i'' j'' -> cvCvtColor i'' j'' $ fromCvtColorFlag f

-- yes i edited this by hand because i did not know how to refer to an anonymous enum
data CvtColorFlag
  = BGR2BGRA 
  | RGB2RGBA 

  | BGRA2BGR
  | RGBA2RGB

  | BGR2RGBA 
  | RGB2BGRA 

  | RGBA2BGR 
  | BGRA2RGB 

  | BGR2RGB  
  | RGB2BGR  

  | BGRA2RGBA
  | RGBA2BGRA

  | BGR2GRAY 
  | RGB2GRAY 
  | GRAY2BGR 
  | GRAY2RGB
  | GRAY2BGRA
  | GRAY2RGBA 
  | BGRA2GRAY
  | RGBA2GRAY 

  | BGR2BGR565 
  | RGB2BGR565
  | BGR5652BGR 
  | BGR5652RGB 
  | BGRA2BGR565 
  | RGBA2BGR565
  | BGR5652BGRA 
  | BGR5652RGBA 

  | GRAY2BGR565
  | BGR5652GRAY

  | BGR2BGR555 
  | RGB2BGR555 
  | BGR5552BGR 
  | BGR5552RGB 
  | BGRA2BGR555
  | RGBA2BGR555 
  | BGR5552BGRA 
  | BGR5552RGBA

  | GRAY2BGR555
  | BGR5552GRAY

  | BGR2XYZ   
  | RGB2XYZ  
  | XYZ2BGR 
  | XYZ2RGB

  | BGR2YCrCb
  | RGB2YCrCb
  | YCrCb2BGR
  | YCrCb2RGB

  | BGR2HSV  
  | RGB2HSV  

  | BGR2Lab   
  | RGB2Lab   

  | BayerBG2BGR
  | BayerGB2BGR
  | BayerRG2BGR
  | BayerGR2BGR

  | BayerBG2RGB
  | BayerGB2RGB
  | BayerRG2RGB
  | BayerGR2RGB

  | BGR2Luv
  | RGB2Luv
  | BGR2HLS
  | RGB2HLS

  | HSV2BGR
  | HSV2RGB

  | Lab2BGR
  | Lab2RGB
  | Luv2BGR
  | Luv2RGB
  | HLS2BGR
  | HLS2RGB

  | BayerBG2BGR_VNG
  | BayerGB2BGR_VNG
  | BayerRG2BGR_VNG
  | BayerGR2BGR_VNG
    
  | BayerBG2RGB_VNG
  | BayerGB2RGB_VNG
  | BayerRG2RGB_VNG
  | BayerGR2RGB_VNG
    
  | BGR2HSV_FULL
  | RGB2HSV_FULL
  | BGR2HLS_FULL 
  | RGB2HLS_FULL 
    
  | HSV2BGR_FULL
  | HSV2RGB_FULL
  | HLS2BGR_FULL
  | HLS2RGB_FULL
    
  | LBGR2Lab
  | LRGB2Lab
  | LBGR2Luv
  | LRGB2Luv
    
  | Lab2LBGR
  | Lab2LRGB
  | Luv2LBGR
  | Luv2LRGB
    
  | BGR2YUV  
  | RGB2YUV    
  | YUV2BGR   
  | YUV2RGB    
    
  | BayerBG2GRAY
  | BayerGB2GRAY
  | BayerRG2GRAY
  | BayerGR2GRAY

  | YUV420i2RGB
  | YUV420i2BGR
  | YUV420sp2RGB
  | YUV420sp2BGR
    
fromCvtColorFlag :: CvtColorFlag -> CInt
fromCvtColorFlag BGR2BGRA    = 0
fromCvtColorFlag RGB2RGBA    = fromCvtColorFlag BGR2BGRA
fromCvtColorFlag BGRA2BGR    = 1
fromCvtColorFlag RGBA2RGB    = fromCvtColorFlag BGRA2BGR
fromCvtColorFlag BGR2RGBA    = 2
fromCvtColorFlag RGB2BGRA    = fromCvtColorFlag BGR2RGBA
fromCvtColorFlag RGBA2BGR    = 3
fromCvtColorFlag BGRA2RGB    = fromCvtColorFlag RGBA2BGR
fromCvtColorFlag BGR2RGB     = 4
fromCvtColorFlag RGB2BGR     = fromCvtColorFlag BGR2RGB
fromCvtColorFlag BGRA2RGBA   = 5
fromCvtColorFlag RGBA2BGRA   = fromCvtColorFlag BGRA2RGBA
fromCvtColorFlag BGR2GRAY    = 6
fromCvtColorFlag RGB2GRAY    = 7
fromCvtColorFlag GRAY2BGR    = 8
fromCvtColorFlag GRAY2RGB    = fromCvtColorFlag GRAY2BGR
fromCvtColorFlag GRAY2BGRA   = 9
fromCvtColorFlag GRAY2RGBA   = fromCvtColorFlag GRAY2BGRA
fromCvtColorFlag BGRA2GRAY   = 10
fromCvtColorFlag RGBA2GRAY   = 11
fromCvtColorFlag BGR2BGR565  = 12
fromCvtColorFlag RGB2BGR565  = 13
fromCvtColorFlag BGR5652BGR  = 14
fromCvtColorFlag BGR5652RGB  = 15
fromCvtColorFlag BGRA2BGR565 = 16
fromCvtColorFlag RGBA2BGR565 = 17
fromCvtColorFlag BGR5652BGRA = 18
fromCvtColorFlag BGR5652RGBA = 19
fromCvtColorFlag GRAY2BGR565 = 20
fromCvtColorFlag BGR5652GRAY = 21
fromCvtColorFlag BGR2BGR555  = 22
fromCvtColorFlag RGB2BGR555  = 23
fromCvtColorFlag BGR5552BGR  = 24
fromCvtColorFlag BGR5552RGB  = 25
fromCvtColorFlag BGRA2BGR555 = 26
fromCvtColorFlag RGBA2BGR555 = 27
fromCvtColorFlag BGR5552BGRA = 28
fromCvtColorFlag BGR5552RGBA = 29
fromCvtColorFlag GRAY2BGR555 = 30
fromCvtColorFlag BGR5552GRAY = 31
fromCvtColorFlag BGR2XYZ     = 32
fromCvtColorFlag RGB2XYZ     = 33
fromCvtColorFlag XYZ2BGR     = 34
fromCvtColorFlag XYZ2RGB     = 35
fromCvtColorFlag BGR2YCrCb   = 36
fromCvtColorFlag RGB2YCrCb   = 37
fromCvtColorFlag YCrCb2BGR   = 38
fromCvtColorFlag YCrCb2RGB   = 39
fromCvtColorFlag BGR2HSV     = 40
fromCvtColorFlag RGB2HSV     = 41
fromCvtColorFlag BGR2Lab     = 44
fromCvtColorFlag RGB2Lab     = 45
fromCvtColorFlag BayerBG2BGR = 46
fromCvtColorFlag BayerGB2BGR = 47
fromCvtColorFlag BayerRG2BGR = 48
fromCvtColorFlag BayerGR2BGR = 49
fromCvtColorFlag BayerBG2RGB = fromCvtColorFlag BayerRG2BGR
fromCvtColorFlag BayerGB2RGB = fromCvtColorFlag BayerGR2BGR
fromCvtColorFlag BayerRG2RGB = fromCvtColorFlag BayerBG2BGR
fromCvtColorFlag BayerGR2RGB = fromCvtColorFlag BayerGB2BGR
fromCvtColorFlag BGR2Luv     = 50
fromCvtColorFlag RGB2Luv     = 51
fromCvtColorFlag BGR2HLS     = 52
fromCvtColorFlag RGB2HLS     = 53
fromCvtColorFlag HSV2BGR     = 54
fromCvtColorFlag HSV2RGB     = 55
fromCvtColorFlag Lab2BGR     = 56
fromCvtColorFlag Lab2RGB     = 57
fromCvtColorFlag Luv2BGR     = 58
fromCvtColorFlag Luv2RGB     = 59
fromCvtColorFlag HLS2BGR     = 60
fromCvtColorFlag HLS2RGB     = 61
fromCvtColorFlag BayerBG2BGR_VNG = 62
fromCvtColorFlag BayerGB2BGR_VNG = 63
fromCvtColorFlag BayerRG2BGR_VNG = 64
fromCvtColorFlag BayerGR2BGR_VNG = 65
fromCvtColorFlag BayerBG2RGB_VNG = fromCvtColorFlag BayerRG2BGR_VNG
fromCvtColorFlag BayerGB2RGB_VNG = fromCvtColorFlag BayerGR2BGR_VNG
fromCvtColorFlag BayerRG2RGB_VNG = fromCvtColorFlag BayerBG2BGR_VNG
fromCvtColorFlag BayerGR2RGB_VNG = fromCvtColorFlag BayerGB2BGR_VNG
fromCvtColorFlag BGR2HSV_FULL = 66
fromCvtColorFlag RGB2HSV_FULL = 67
fromCvtColorFlag BGR2HLS_FULL = 68
fromCvtColorFlag RGB2HLS_FULL = 69
fromCvtColorFlag HSV2BGR_FULL = 70
fromCvtColorFlag HSV2RGB_FULL = 71
fromCvtColorFlag HLS2BGR_FULL = 72
fromCvtColorFlag HLS2RGB_FULL = 73
fromCvtColorFlag LBGR2Lab     = 74
fromCvtColorFlag LRGB2Lab     = 75
fromCvtColorFlag LBGR2Luv     = 76
fromCvtColorFlag LRGB2Luv     = 77
fromCvtColorFlag Lab2LBGR     = 78
fromCvtColorFlag Lab2LRGB     = 79
fromCvtColorFlag Luv2LBGR     = 80
fromCvtColorFlag Luv2LRGB     = 81
fromCvtColorFlag BGR2YUV      = 82
fromCvtColorFlag RGB2YUV      = 83
fromCvtColorFlag YUV2BGR      = 84
fromCvtColorFlag YUV2RGB      = 85
fromCvtColorFlag BayerBG2GRAY = 86
fromCvtColorFlag BayerGB2GRAY = 87
fromCvtColorFlag BayerRG2GRAY = 88
fromCvtColorFlag BayerGR2GRAY = 89
fromCvtColorFlag YUV420i2RGB  = 90
fromCvtColorFlag YUV420i2BGR  = 91
fromCvtColorFlag YUV420sp2RGB = 92
fromCvtColorFlag YUV420sp2BGR = 93
