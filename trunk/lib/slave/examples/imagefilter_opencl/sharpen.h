/*sharpen.h 
  Author:Christofer Ferm
  LoPEC Low Power Erlang-based Cluster*/ 

#include "read.h" 

int sharpen(char *filename);

/*init OpenCL Stuff*/ 
int init_OpenCL(); 

/*do the magic*/
int do_calc_OpenCL();
