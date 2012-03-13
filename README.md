# HOpenCV

[OpenCV](http://opencv.willowgarage.com/wiki/) bindings for Haskell
(tested with OpenCV 2.1, 2.2, 2.3.0, and 2.3.1).

- Image color channel count and color depth are statically checked.

- A functional interface is provided through the HighCV module.

- When operations are directly composed, they will be performed
  in-place where possible as the intermediate images are not
  observable. GHC's optimizations must be enabled (e.g. -O2).

- See
  [src/Examples](https://github.com/acowley/HOpenCV/tree/master/src/Examples)
  for example programs. In particular, the
  [VideoFunhouse](https://github.com/acowley/HOpenCV/tree/master/src/Examples/VideoFunhouse)
  executable demonstrates realtime image processing on either the
  video feed from a webcam or a video file. Fusion of in-place
  operations is demonstrated along with light-weight parallelism.

NOTE: Only a small part of OpenCV is currently wrapped.


