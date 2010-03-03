#include "HOpenCV.h"

void lib_test()
{
    CvCapture *capture = new_capture(0);
    IplImage *frame = NULL;
    new_window(0,1);

    while (1) {
        frame = cvQueryFrame(capture);
        //show_image(0, frame);
        wait_key(30);
    }

    del_window(0);
    del_capture(capture);
}

void direct_test()
{
    CvCapture *capture = new_capture(0);
    cvNamedWindow("blah",1);

    while (1) {
        cvShowImage("blah", query_frame(capture));
        cvWaitKey(10);
    }

}

int main(int argc, char *argv[])
{
//    direct_test();
    lib_test();
}
