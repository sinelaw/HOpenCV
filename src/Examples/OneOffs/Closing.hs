import OpenCV.HighCV

main = toFile "closed.png" . erode 8 . dilate 8 =<< fromFileGray "input.png"