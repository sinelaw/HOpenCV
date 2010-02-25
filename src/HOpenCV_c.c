#include <cv.h>
#include <highgui.h>


/***************/
/* Capture devices */
CvCapture* new_capture(int dev_number) {
    return cvCaptureFromCAM(dev_number);
}

void del_capture(CvCapture **capture) {
    cvReleaseCapture( capture );
    *capture = NULL;
}


IplImage *query_frame(CvCapture *capture) {
    return cvQueryFrame( capture );
}

