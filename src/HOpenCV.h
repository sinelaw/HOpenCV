#include <cv.h>
#include <highgui.h>


/***************/
/* Capture devices */
CvCapture* new_capture(int dev_number);

void del_capture(CvCapture **capture);


IplImage *query_frame(CvCapture *capture) ;
