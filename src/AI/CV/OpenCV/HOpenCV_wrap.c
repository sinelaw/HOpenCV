#include <cxcore.h>
#include <cv.h>
#include <highgui.h>

#include <stdio.h>

void debug_print_image_header(IplImage *image)
{
    printf("IplImage: \n"
           "\tnSize: %d\n"
           "\tID: %d\n"
           "\tnChannels: %d\n"
           "\talphaChannel: %d\n"
           "\tdepth: %d\n"
           "\tdataOrder: %d\n"
           "\torigin: %d\n"
           "\talign: %d\n"
           "\twidth: %d\n"
           "\theight: %d\n"
           "\timageSize: %d\n"
           "\twidthStep: %d\n",
           image->nSize, image->ID, image->nChannels, image->alphaChannel,
           image->depth, image->dataOrder, image->origin, image->align,
           image->width, image->height, image->imageSize, image->widthStep);
}

/****************************************************************************/

void release_capture(CvCapture *capture)
{
    CvCapture *temp = capture;
    cvReleaseCapture(&temp);
}

/****************************************************************************/

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

/**********************************************************/

/* Haskell's FFI doesn't know how to pass structs by value, I think. */
IplImage *create_image(int width, int height, int depth, int channels)
{
    return cvCreateImage(cvSize(width,height), depth, channels);
}

void release_image(IplImage *image)
{
    IplImage *temp = image;
    return cvReleaseImage(&temp);
}

/* Notice that according to opencv docs, cvGetSize returns the ROI for images, not the image.width and .height */
void get_size(const CvArr *arr, CvSize *size)
{
    *size = cvGetSize(arr);
}

/* TODO instead of get_depth, get_nChannels maybe we should allow serialization of the IplImage struct itself, as we do
   for CvSize? */
int get_depth(const IplImage *image)
{
    return image->depth;
}

int get_nChannels(const IplImage *image)
{
    return image->nChannels;
}

void dilate(const CvArr *src, CvArr *dest, int iterations)
{
    cvDilate(src, dest, NULL, iterations);
}

/**********************************************************/

void release_mem_storage(CvMemStorage *mem_store)
{
    CvMemStorage *temp = mem_store;
    cvReleaseMemStorage(&temp);
}

void cv_free(void *obj)
{
    void *temp = obj;
    cvFree(&temp);
}

/****************************************************************************/

int seq_total(const CvSeq *seq) {
    return seq->total;
}

/* Commonly used case of CV_GET_SEQ_ELEM is CvRect-typed elements.
   The macro CV_GET_SEQ_ELEM is supposed to be faster in some cases
   than the function cvGetSeqElem. */
/*CvRect *c_rect_cvGetSeqElem(const CvSeq *seq, int index) {
    return CV_GET_SEQ_ELEM( CvRect, seq, index);
    }*/


void c_cvRectangle(CvArr *img, int x, int y, int width, int height)
{
    cvRectangle(img, cvPoint(x,y),
                     cvPoint((x+width),
                             (y+height)),
                CV_RGB(255,0,0), 3 , 8, 0);
}

/****************************************************************************/

CvSeq *c_cvHaarDetectObjects( const CvArr* image,
                              CvHaarClassifierCascade* cascade,
                              CvMemStorage* storage, double scale_factor,
                              int min_neighbors , int flags,
                              int width, int height)
{
    return cvHaarDetectObjects(image, cascade, storage, scale_factor, min_neighbors, flags, cvSize(width, height), cvSize(0, 0));
}

/****************************************************************************/

void doNothing( int newPos ) { return; }

int wrap_createTrackbar( const char* trackbarName
                       , const char* winName
                       , int*        startPos
                       , int         maxVal)
{
  return cvCreateTrackbar(trackbarName, winName, startPos, maxVal, doNothing);
}

/****************************************************************************/

CvVideoWriter *
wrap_cvCreateVideoWriter( const char *file
                        , int fourcc
                        , double fps
                        , int height, int width)
{
  return cvCreateVideoWriter(file, fourcc, fps, cvSize(width, height), 1);
}
