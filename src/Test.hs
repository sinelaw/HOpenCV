module Main where


import qualified HOpenCV

main :: IO ()
main = do
  capture <- HOpenCV.newCapture 0
  HOpenCV.newWindow 0 1
  let showFrames = do
         frame <- HOpenCV.queryFrame capture
         HOpenCV.showImage 0 frame
         HOpenCV.waitKey 10
         showFrames
         return ()
  showFrames
  HOpenCV.delWindow 0
  HOpenCV.delCapture capture
  return ()

