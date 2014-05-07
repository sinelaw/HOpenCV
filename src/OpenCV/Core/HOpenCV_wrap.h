#ifdef OCV21
#include <opencv/cv.h>
#include <opencv/highgui.h>
#else
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/core/core_c.h>
#endif

void debug_print_image_header(IplImage *image);

void release_capture(CvCapture *capture);
void release_video_writer(CvVideoWriter *writer);

    
void new_window(int num, int flags);
void del_window(int num);
void show_image(int num, IplImage *image);

IplImage *create_image(int width, int height, int depth, int channels);
void release_image(IplImage *image);

void get_size(const CvArr *arr, CvSize *size);
int get_depth(const IplImage *image);
int get_nChannels(const IplImage *image);

void dilate(const CvArr *src, CvArr *dest, int iterations);

void release_mem_storage(CvMemStorage *mem_store);
void cv_free(void *obj);

int seq_total(const CvSeq *seq);
/* CvRect *c_rect_cvGetSeqElem(const CvSeq *seq, int index); */

void c_cvGetSeqPoint(const CvSeq *seq, int i, int*x, int*y);


CvVideoWriter* cvCreateVideoWriter(const char* filename, int fourcc, 
        double fps, int frame_x, int frame_y, int is_color);

void c_cvRectangle(CvArr *img, int x, int y, int width, int height);

void c_cvLine(CvArr *img, int x1, int y1, int x2, int y2, double r, double g, 
              double b, int thickness, int lineType, int shift);

void c_cvSetRoi(IplImage* img, int x, int y, int width, int height);
void c_cvGetROI(IplImage* img, int* rptr);

void c_cvPutText(CvArr *img, const char* msg, int x, int y, 
                 double r, double g, double b);

int c_cvFindContours(CvArr *img, CvMemStorage *storage, CvSeq** first_contour, 
                     int header_size, int mode, int method, int offset_x, 
                     int offset_y);

double c_cvContourArea( const CvArr *contour);
double c_cvContourPerimeter( const void* contour);

void c_cvAvg(const CvArr *img, const CvArr *mask, CvScalar* avg);

CvSeq *c_cvHaarDetectObjects( const CvArr* image,
                              CvHaarClassifierCascade* cascade,
                              CvMemStorage* storage, double scale_factor,
                              int min_neighbors , int flags,
                              int width, int height);


void c_cvDrawContours( CvArr * img, CvSeq* contour,
      CvScalar * external_color,
      CvScalar * hole_color,
      int max_level, int max_thickness, int line_type, CvPoint * offset );
