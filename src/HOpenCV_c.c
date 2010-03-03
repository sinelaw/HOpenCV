#include <cv.h>
#include <highgui.h>

#include <stdio.h>

/***************/
/* Capture devices */
CvCapture* new_capture(int dev_number) {
    return cvCaptureFromCAM(dev_number);
}

void del_capture(CvCapture *capture) {
    cvReleaseCapture( &capture );
}


IplImage *query_frame(CvCapture *capture) {
    return cvQueryFrame( capture );
}

IplImage *query_cloned_frame(CvCapture *capture) {
    return cvCloneImage(cvQueryFrame( capture ));
}


/*************/
void num_to_name(int num, char *name, int length)
{
    snprintf(name, length, "win-%d", num);
}

void new_window(int num, int flags)
{
    char name[100];
    num_to_name(num, name, sizeof(name));
    cvNamedWindow(name, flags);
}

void del_window(int num)
{
    char name[100];
    num_to_name(num, name, sizeof(name));
    cvDestroyWindow(name);
}

void show_image(int num, IplImage *image)
{
    char name[100];
    num_to_name(num, name, sizeof(name));
    cvShowImage(name, image);
}

void wait_key(int delay)
{
    cvWaitKey(delay);
}


IplImage *clone_image(IplImage *image)
{
    return cvCloneImage(image);
}

void del_image(IplImage *image)
{
    cvReleaseImage(&image);
}

void dilate(IplImage *src, int iterations, IplImage *dest)
{
    cvDilate(src, dest, NULL, iterations);
}
