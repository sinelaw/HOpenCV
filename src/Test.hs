module Main where


import qualified HOpenCV

main :: IO ()
main = do
  capture <- HOpenCV.newCapture 0
  frame <- HOpenCV.queryFrame capture
  return ()

