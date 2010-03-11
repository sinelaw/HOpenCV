#include <cv.h>


void debug_print_image_header(IplImage *image);

void release_capture(CvCapture *capture);

    
void new_window(int num, int flags);
void del_window(int num);
void show_image(int num, IplImage *image);

IplImage *create_image(int width, int height, int depth, int channels);
void release_image(IplImage *image);

void get_size(const CvArr *arr, CvSize *size);
int get_depth(const IplImage *image);
int get_nChannels(const IplImage *image);

void dilate(int iterations, const CvArr *src, CvArr *dest);
