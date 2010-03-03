#include <cv.h>
#include <highgui.h>


CvCapture* new_capture(int dev_number);

void del_capture(CvCapture *capture);

IplImage *query_frame(CvCapture *capture);
IplImage *query_cloned_frame(CvCapture *capture);

void new_window(int num, int flags);

void del_window(int num);

void show_image(int num, IplImage *image);

void wait_key(int delay);

IplImage *clone_image(IplImage *image);

void del_image(IplImage *image);

void dilate(IplImage *src, int iterations, IplImage *dest);
