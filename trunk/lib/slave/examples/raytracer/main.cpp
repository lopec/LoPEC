#include <CL/cl.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <limits>
#include <cmath>
#include <list>
#include <fstream>


using namespace std;

typedef struct{
  cl_float4 c; 

}centers; 

cl_float *center = (cl_float *)malloc(sizeof(cl_float)*4);
cl_float *light = (cl_float*)malloc(sizeof(cl_float)*4);  
cl_float *origin = (cl_float*)malloc(sizeof(cl_float)*4);
cl_float *r;
cl_int *colorid;
cl_int numobj;  


typedef struct{
  cl_float x;
  cl_float y;
  cl_float z; 
}centervals; 




typedef struct{
  cl_int r; 
  cl_int g;
  cl_int b; 
}color;

typedef struct{
  cl_float r;
  cl_float4 center; 
  cl_int color; 
}sphere; 

/*typedef struct{
  sphere *list; 
  cl_float4 light; 
  cl_float4 origin;
  cl_int numobj;     
}scene; 
*/

typedef struct{
  cl_float4 center[]; 
  cl_float r[]; 
  cl_int colorid[]; 
  cl_float4 light; 
  cl_float4 origin; 
  cl_int numobj; 
}scene; 

typedef struct{
  cl_int colid; 
  cl_float val; 
}pixel; 


void printArray(const cl_float *olol, cl_uint count, const char *s);
void printResults(const cl_float *res, cl_int *col, cl_uint count); 
//void printResults(const pixel *res, cl_uint count);
// Use a static data size for simplicity
//

  

#define DATA_SIZE (1)
#define OUTPUT_SIZE (1024*1024)
int colorvals[3] = {255,255,0}; 
color map[4];


//assign a float4 array to another 
void float4Assign(cl_float *to, cl_float from[4])
{

  

 for(int i=0; i<4; i++)
    {
      to[i] = from[i];
    }
 
}

void addCenterToList(cl_float *item)
{
  for(int i=0; i<4; i++)
    {
      center[(numobj*4)+i] = item[i]; 
    }
  
}

void printOutFloat4(cl_float in[4])
{
  for(int i=0; i<4;  i++)
    {
      cout << in[i] << endl; 

    }

}

// create a new Sphere 
sphere newSphere(cl_float r, cl_float4 center)
{

  sphere s; 

  s.r = r; 
  //float4Assign(&s.center, center); 

  return s; 

}

color newColor(int r, int g, int b)
{
  color c; 

  c.r = r; 
  c.g = g; 
  c.b = b; 

  return c;

}


color getColor(cl_int id)
{
  
  return map[id]; 

}


void readScene()
{
  
  int scenesize = 3; 

  
  r = (cl_float*)malloc(sizeof(cl_float)*scenesize); 
  colorid = (cl_int*)malloc(sizeof(cl_int)*scenesize);
  numobj = 0; 
  cl_float *inlight = (cl_float *)malloc(sizeof(cl_float)*4);
  inlight[0]  = 0; 
  inlight[1]  =-3;
  inlight[2]  = 2; 
  inlight[3]  = 0; 
  cl_float *inorig  = (cl_float *)malloc(sizeof(cl_float)*4);
  inorig[0] = 0; 
  inorig[1] = 0;
  inorig[2] = -4; 
  inorig[0] = 0; 

  // Fix this function
  //printOutFloat4(inlight);  

  // Fix this function
  float4Assign(light, inlight);
  float4Assign(origin, inorig); 
  
  //Fix this function
  //printOutFloat4(light);
  // cl_float **centerlist = (cl_float **)malloc(sizeof(cl_float*)*3);
  /* for(int i=0; i < 4; i++)
    {
      center[i] = (cl_float *)malloc(sizeof(cl_float)*4);
      }*/
  cl_float *center1 =  (cl_float *)malloc(sizeof(cl_float)*4);
  center1[0] = 0;
  center1[1] = 0;
  center1[2] = -1;
  center1[3] = 0; 
  cl_float *center2 = (cl_float *)malloc(sizeof(cl_float)*4);
  center2[0] = 1;
  center2[1] = 0;
  center2[2] = 0;
  center2[3] = 0;
  cl_float *center3 = (cl_float *)malloc(sizeof(cl_float)*4);
  center3[0] = -0.5;
  center3[1] = 0;
  center3[2] = 0;
  center3[3] = 0;

  

    
  
   addCenterToList(center1); 
  r[numobj] = 0.25;
  colorid[numobj] = 1;
  numobj++;
  addCenterToList(center2); 
  r[numobj] = 0.5; 
  colorid[numobj]=2; 
  numobj++; 
  addCenterToList(center3); 
  r[numobj] = 0.5; 
  colorid[numobj]=3; 
  numobj++; 
  //printOutFloat4(center[0]); 
  

  /*  sphere sp = newSphere(0.25,center); 
  sphere sp1 = newSphere(0.5,center1); 
  sphere sp2 = newSphere(0.5,center2);

  float4Assign(&s.center[s.numobj],sp.center); 
  s.r[s.numobj] = sp.r;
  cout << s.r[s.numobj] << " successfully added " << endl;
  s.colorid[s.numobj] = 1; 
  s.numobj++; 
  float4Assign(&s.center[s.numobj], sp1.center); 
  s.r[s.numobj]  = sp1.r; 
  cout << s.r[s.numobj] << " successfully added " << endl;
  s.colorid[s.numobj] = 2; 
  s.numobj++; 
   s.list[s.numobj] = sp2; 
     s.numobj++;
  ins->r[0] = s.r[0];
  ins->r[1] = s.r[1]; 
  //float4Assign(&ins->center,s.center); 
  float4Assign(&ins->light,s.light); 
  float4Assign(&ins->origin,s.origin); 
  ins->colorid[0] = s.colorid[0];
  ins->colorid[1] = s.colorid[1];*/ 

}


int main(int argc, char* argv[]){

bool write_header = true;
  if(argc < 5)
    {
      cout << "Usage: <path to OpenCL code> <startline> <stopline> <total lines>" << endl; 
      exit(0);
    }

  if(argc == 8)
    {
  
      colorvals[0] = atoi(argv[5]); 
      colorvals[1] = atoi(argv[6]); 
      colorvals[2] = atoi(argv[7]);
  
    }
  //size_t global, local;
  int err;

  // cl_int data[3] ={atoi(argv[2]),atoi(argv[3]),atoi(argv[4])};
  //  cl_uint results[DATA_SIZE];
  int output_size = OUTPUT_SIZE;
  // int print_out =  (argv[4]) * (atoi(argv[2]) - atoi(argv[3]));
  // cl_float *results2 =  (cl_float*)malloc(OUTPUT_SIZE*sizeof(cl_float)); 
  // pixel *results2 = (pixel*)malloc(OUTPUT_SIZE*sizeof(pixel)); 
  // OpenCL stuffs
   cl_device_id device_id;
   cl_context context;
   cl_command_queue queue;
  cl_program program;
  cl_kernel kernel;
  //  cl_mem input; 
  // cl_mem output;
  //cl_mem posmem; 
  cl_int count = atoi(argv[4]);
   
  cl_int pos =0;
  //cl_int *colors = (cl_int*)malloc(OUTPUT_SIZE*sizeof(cl_int));

  

  map[0]=newColor(0,0,0); 
  map[1]=newColor(255,0,0);
  map[2]=newColor(0,255,0); 
  map[3]=newColor(0,0,255); 
 
 
 
  //count = DATA_SIZE;

  
   
/** 
      read the opencl code from file 


*/


  FILE *file;  

  file = fopen(argv[1],"r"); // open in text mode
  
  if(file == NULL)
    {
      cout << "could not load file " << argv[1] <<  endl; 

    }

  fseek(file, 0L, SEEK_END); 
  long filelen = ftell(file); 
  rewind(file); 

  const char *KernelSource = (char*)calloc(filelen + 1, sizeof(char));  

  if(KernelSource == NULL)
    {
      cout << "insufficient memory to read file into memory" << endl; 
      return 0;
    }
     
  size_t readlength;

  readlength = fread((void*)KernelSource,filelen, 1, file); 
  

    /**
	 Fetches OpenCL device IDs from the system corresponding to the arguments,
	 First argument is something to look deeper into :p
	 Second argument signals which kind of device to query
	 Third is the max number of devices you want to fetch IDs from
	 Forth is a device array where the results are written, since the third
	 argument is only one in this case, we'll only expect one result,
	 Fifth argument gets the number of devices matching device type written to
	 it, or, as in this case, if the argument is NULL it is ignored.
      */
      err = clGetDeviceIDs(NULL, CL_DEVICE_TYPE_GPU, 1, &device_id, NULL);

      if (err != CL_SUCCESS){
	cout << "clGetDeviceIDs :(" << err << endl;
	exit(1);
      }

      /**
	 Creates the OpenCL context.

	 First argument is a list of properties, look further into,
	 Second specifies how many devices that will be passed as the third arg,
	 Third argument is the list of devices,
	 Forth argument is a callback function that errors will be reported to,
	 or as in this case, it can be NULL, research it further,
	 Fifth is related to the forth,
	 Sixth might be an error code, hopefully not.
      */
      context = clCreateContext(NULL, 1, &device_id, NULL, NULL, &err);

      if (err != CL_SUCCESS){
	cout << "clCreateContext :( " << err << endl;
	exit(1);
      }

      /**
	 Creates the command queue.

	 Most arguments are self-descriptive except for the third which is a list
	 of properties if you'd want that. The properties can be out-of-order
	 execution or command queue profiling.
      */
      queue = clCreateCommandQueue(context, device_id, NULL, &err);

      if (err != CL_SUCCESS){
	cout << "clCreateCommandQueue :( " <<  err << endl;
	exit(1);
      }

      /**
	 Load the program somewheres
      */
      program = clCreateProgramWithSource(context, 1, &KernelSource, NULL, &err);

      if (err != CL_SUCCESS){
	cout << "clCreateProgramWithSource :( %d" << err << endl;
	exit(1);
      }

      /**
	 Build it :O
      */
      err = clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);

      size_t len;
      char buffer[2048];
      clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG,
			sizeof(buffer), buffer, &len);
  
      // cout << buffer << endl;


      if (err != CL_SUCCESS){
	cout << "clBuildProgram" << err << endl;
	cout << buffer << endl;
	exit(1);
      }

      /**
	 Instansiate the kernel.
      */
      kernel = clCreateKernel(program, "hello", &err);

      if (err != CL_SUCCESS){
	cout << "clCreateKernel :( " << err << endl;
	exit(1);
      }



      cl_int y = count-1;    
     
      cl_int data[3] = {y,0,atoi(argv[4])}; 
       cl_int start = atoi(argv[2]);
      cl_int stop  = atoi(argv[3]);

      if(write_header)
	{
	  
	  cout << "P3\n" << count << " " << start-stop << "\n" << "255" << "\n";
        }
      
      readScene();
            
      //cl_mem centermem;
      //cl_mem radiusmem;
      //cl_mem colorinmem;
      //printOutFloat4(light);
      
      //printOutFloat4(center);
      
      
      data[1] = numobj;
     

  for(y=start-1; y>=stop; y--)
    {

     
     

      //cl_float *result = (cl_float*)malloc((count*sizeof(cl_float))*s.numobj); 
      //   cl_float result[numobj][count];

      cl_int *colors = (cl_int*)malloc(count*sizeof(cl_int));
      /* for(int i=0; i<s.numobj; i++)
      	{
      */
     
    
      data[0] = y;    
	   cl_float *results2 =  (cl_float*)malloc(count*sizeof(cl_float));
	
	  size_t global, local;


	  cl_mem input; 
	  cl_mem output;
	  cl_mem centermem;
	  cl_mem radiusmem;
	  cl_mem colorinmem;
	  cl_mem posmem;
	  cl_mem lightmem; 
	  cl_mem origmem; 
	  

      
	  // for(int i=0; i<count; i++)
	  // {
	  // results2[i]=0;
      
      
	  // }

 

	  /**
	     Create buffers
	  */
	  input = clCreateBuffer(context, CL_MEM_READ_WRITE,
			 sizeof(cl_int) * 3 , NULL, &err);
	  if (err != CL_SUCCESS){
	    cout << "clCreateBufferinput: " << err << endl;
	    exit(1);
	  }
	  
	   centermem = clCreateBuffer(context, CL_MEM_READ_WRITE,
				      sizeof(cl_float)*(numobj*4),NULL, &err); 

	  if(err != CL_SUCCESS){

	    cout << "clCreateBufferScene: " << err << endl; 
	    exit(1); 
	    }

	  radiusmem = clCreateBuffer(context, CL_MEM_READ_WRITE,
				    sizeof(cl_float)*numobj,NULL, &err); 

	  if(err != CL_SUCCESS){

	    cout << "clCreateBufferScene: " << err << endl; 
	    exit(1); 
	    }

	  colorinmem = clCreateBuffer(context, CL_MEM_READ_WRITE,
				    sizeof(cl_int)*numobj,NULL, &err); 

	  if(err != CL_SUCCESS){

	    cout << "clCreateBufferScene: " << err << endl; 
	    exit(1); 
	    }


	  output = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR,
			  sizeof(cl_float) * count, NULL, &err);
	  if (err != CL_SUCCESS){
	    cout << "clCreateBufferoutput: " << err << endl;
	    exit(1);
	  }

	  

   
 
	  posmem = clCreateBuffer(context, CL_MEM_READ_WRITE,
	 	  sizeof(cl_int)*count, NULL, &err);

	   if( err != CL_SUCCESS)
	    {
	      cout << "clCreateBuffer: " << err << endl;
	      exit(1);
	    }

	   lightmem = clCreateBuffer(context, CL_MEM_READ_WRITE,
	 	  sizeof(cl_float)*4, NULL, &err);

	   if( err != CL_SUCCESS)
	    {
	      cout << "clCreateBuffer: " << err << endl;
	      exit(1);
	    }

	   origmem = clCreateBuffer(context, CL_MEM_READ_WRITE,
	 	  sizeof(cl_float)*4, NULL, &err);

	   if( err != CL_SUCCESS)
	    {
	      cout << "clCreateBuffer: " << err << endl;
	      exit(1);
	    }
    
	  //  printArray((cl_uint *)input, count, "Input: ");
	  /**
	     Write stuff to the input buffer.
	  */
	  err = clEnqueueWriteBuffer(queue, input, CL_TRUE, 0, sizeof(cl_int) * 3,
				 &data, 0, NULL, NULL);

	  err  = clEnqueueWriteBuffer(queue, centermem, CL_TRUE, 0, sizeof(cl_float)*(numobj*4),
				      center, 0, NULL, NULL);

	  err  = clEnqueueWriteBuffer(queue, radiusmem, CL_TRUE, 0, sizeof(cl_float)*numobj, 
				      r, 0, NULL, NULL); 

	  err  = clEnqueueWriteBuffer(queue, colorinmem, CL_TRUE, 0, sizeof(cl_float)*numobj,
				      colorid, 0, NULL, NULL); 
	  
	  err = clEnqueueWriteBuffer(queue, lightmem, CL_TRUE, 0, sizeof(cl_float)*4,
				     light, 0, NULL, NULL);

	  err = clEnqueueWriteBuffer(queue, origmem, CL_TRUE, 0, sizeof(cl_float)*4,
				     origin, 0, NULL, NULL);

	  /*
	    Set kernel arguments
	  */
	  clSetKernelArg(kernel, 0, sizeof(cl_mem),  &input);
	  clSetKernelArg(kernel, 1, sizeof(cl_mem), &centermem);
	  clSetKernelArg(kernel, 2, sizeof(cl_mem), &radiusmem);
	  clSetKernelArg(kernel, 3, sizeof(cl_mem), &colorinmem); 
	  clSetKernelArg(kernel, 4, sizeof(cl_mem), &lightmem);
	  clSetKernelArg(kernel, 5, sizeof(cl_mem), &origmem); 
	  clSetKernelArg(kernel, 6, sizeof(cl_mem),  &output);
	  clSetKernelArg(kernel, 7, sizeof(cl_int), &count);
	  clSetKernelArg(kernel, 8, sizeof(cl_mem), &posmem);
	  

      

 

	  local = 1;

 
 
	  /**
	     Execute the kernel over the entire range of the data set.
	  */


	  global = count;
	  err = clEnqueueNDRangeKernel(queue, kernel, 1, NULL , &global, &local,
			 0, NULL, NULL);
  
	  if(err != CL_SUCCESS)
	    {
	      cout << "could not execute Kernel Error COde:" << err << endl;  
	      exit(1);
	    }



 
	  /**
	     Wait for the command queue to get serviced before reading back results
	  */
	  clFinish(queue);
  
	  /**
	     Read the results from the device
	  */

 

	  err = clEnqueueReadBuffer(queue, output, CL_TRUE, 0,
				    sizeof(cl_float)*count, results2 , 0, NULL, NULL );
	  
  

	  if (err != CL_SUCCESS){
	    cout << "clEnqueueReadBuffer1: " << err << " os:" << output_size << endl;
	    exit(1);
	  }


	    err = clEnqueueReadBuffer(queue, posmem, CL_TRUE, 0,
			    sizeof(cl_int)*count , colors, 0, NULL, NULL); 

	  if (err != CL_SUCCESS)
	    {
	      cout << " clEnqueueReadBuffer2:" << err << endl;
	      exit(1); 

	      }
   
	  printResults(results2,colors,count); 
	  //clean up
	  clReleaseMemObject(posmem);
	  clReleaseMemObject(input);
	  clReleaseMemObject(output);
	  clReleaseMemObject(centermem);
	  clReleaseMemObject(radiusmem); 
	  clReleaseMemObject(colorinmem); 
	  clReleaseMemObject(lightmem);
	  clReleaseMemObject(origmem);
           
       
    }


    // Shut down and clean up
    clReleaseContext(context);
    clReleaseProgram(program);
    clReleaseCommandQueue(queue);
    clReleaseKernel(kernel);
   
   
  
  return 0;
}


int getColorValue(int max, int value)
{
  
  if(value<=max)
      return value;
  else
      return max; 

}




void printResults(const cl_float *res, cl_int *col,cl_uint count)
{
  // cout << "P2\n" << int(sqrt(count)) << " " << int(sqrt(count)) << "\n255\n";

 
  for(cl_uint i=0;  i<count; i++)
    {
      int num = int(.5 + 255. * res[i]/16);
      // int num  = int(res[i]);
      
      if(num<0)
      {
        num = num * -1; 
      }
     
      
     

       color c = getColor(col[i]);
      
      
       int r = getColorValue(c.r,num); 
       int g = getColorValue(c.g,num); 
       int b = getColorValue(c.b,num);  
      //cout << res[0][0 << endl;
      // cout << col[i] << " " << num  << " RGB" << r << " " << g << " " << b <<  endl;
       cout << r << " " << g << " "  << b <<  "\n";
       //cout << res[i] << endl;
       //cout << num << "  " <<  i <<  endl;
	

    }


}


