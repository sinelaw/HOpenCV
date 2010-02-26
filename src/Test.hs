module Main where


import qualified HOpenCV

main :: IO ()
main = do
  capture <- HOpenCV.newCapture 0
  HOpenCV.newWindow 0 1
  frame <- HOpenCV.queryFrame capture
  dilated <- HOpenCV.cloneImage frame
  let showFrames = do
         frame <- HOpenCV.queryFrame capture
         HOpenCV.dilate frame 6 dilated
         HOpenCV.showImage 0 dilated
         HOpenCV.waitKey 2
         showFrames
         return ()
  showFrames
  HOpenCV.delWindow 0
  HOpenCV.delCapture capture
  return ()

