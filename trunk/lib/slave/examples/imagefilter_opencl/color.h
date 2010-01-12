/*color.h 
Author:Christofer Ferm
LoPEC Low Power Erlang-based Cluster 
*/ 

/*Color datatype to hold RGB values*/ 
#include <CL/cl.h>

typedef struct{
  cl_int r; 
  cl_int g;
  cl_int b; 
}color;

/*outputs image as ascii to stdout*/  
int outputImage(color *image, int width, int height); 


/*Creates a new color*/
color newColor(int r, int g, int b);

/*converts a color array to a float array*/

void color2float(color *in , cl_float *outr,cl_float *outg,cl_float *outb, int width, int height);

color *float2color(cl_float *in , color *out, int width, int height);

/*outputs image as ascii to stdout*/  
int outputImage_float(cl_float *r, cl_float *g, cl_float *b, int width, int height);

/*print header*/
int printHeader(int width, int height);

/*put A pixels to result array */
color *putPixels(color *res, cl_float *r, cl_float *g, cl_float *b, int x, int width, int height);

/*convert color to int */ 
int *color2intAlpha(color *in, int *out, int width, int height);

/*convert an int array to color array*/
color *intAlpha2Color(int *in, color *out, int width, int height);
