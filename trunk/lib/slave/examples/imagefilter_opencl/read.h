/*read.h
  Author:Christofer Ferm 
  LoPEC Low Power Erlang-based Cluster*/ 

#include "main.h"

/*reads tha header of the ppm file to get image size*/
int read_header(char *filename, int *width, int *height);

/*reads a ppm file*/ 
int read_ppm(char *filename,color* image, int *image_width, int *image_height);
/*reads OpenCL code from file*/ 
int read_OpenCL(char *filename,char **KernelSource); 



