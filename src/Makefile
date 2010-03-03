all: Test test-c test-cpp

Test: Test.hs HOpenCV_c.c HOpenCV.h HOpenCV.chs
	c2hs  --cppopts='-I/usr/include/opencv' -l HOpenCV.chs
	ghc --make Test.hs HOpenCV_c.c  -lcv -lhighgui -I/usr/include/opencv  -o Test -O2


test-c: test.c HOpenCV_c.c HOpenCV.h
	gcc test.c HOpenCV_c.c -lcv -lhighgui -lcxcore   -I/usr/include/opencv -o test-c -O2

test-cpp: test2.cpp 
	gcc test2.cpp -lcv -lhighgui -lcxcore   -I/usr/include/opencv -o test-cpp -O2