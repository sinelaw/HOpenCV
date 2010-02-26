#include "HOpenCV.h"

void lib_test()
{
    CvCapture *capture = new_capture(0);
    new_window(0,1);

    while (1) {
        show_image(0, query_frame(capture));
        wait_key(20);
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
