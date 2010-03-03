#include <cv.h>
#include <highgui.h>

#include <stdio.h>


void release_capture(CvCapture *capture)
{
    CvCapture *temp = capture;
    cvReleaseCapture(&temp);
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

/*void dilate(IplImage *src, int iterations, IplImage *dest)
{
    cvDilate(src, dest, NULL, iterations);
    }*/
