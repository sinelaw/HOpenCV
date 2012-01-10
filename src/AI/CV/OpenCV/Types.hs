module AI.CV.OpenCV.Types where


import AI.CV.OpenCV.CxCore
import AI.CV.OpenCV.HighGui

import Foreign

type PImage = Ptr IplImage
type PCapture = Ptr CvCapture

type FPImage = ForeignPtr IplImage
type FPCapture = ForeignPtr CvCapture
