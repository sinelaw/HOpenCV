
Test: Test.hs HOpenCV_c.c HOpenCV.chs
	c2hs  --cppopts='-I/usr/include/opencv' -l HOpenCV.chs
	ghc --make Test.hs HOpenCV_c.c  -lcv -lhighgui -I/usr/include/opencv  -o Test -O2

