/*
main.c 
A Raytracer using OpenCL for
LoPEC Low Power Erlang-based Cluster 
Author: Christofer Ferm  
*/
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
#include <sys/wait.h>
#include <sys/types.h>


using namespace std;

typedef struct{
  cl_float4 c; 

}centers; 

/*a list of center arrays for the spheres*/ 
cl_float *center = (cl_float *)malloc(sizeof(cl_float)*4);
/*store the light vector */ 
cl_float *light = (cl_float*)malloc(sizeof(cl_float)*4);  
/*store the position of the origin*/ 
cl_float *origin = (cl_float*)malloc(sizeof(cl_float)*4);
/*store the radius of the spheres*/ 
cl_float *r = (cl_float*)malloc(sizeof(cl_float)); 
/*store the color id's of the spheres */ 
cl_int *colorid = (cl_int*)malloc(sizeof(cl_int)); 
/*will contian how many objects we have in the scene*/ 
cl_int numobj;  



/*Color datatype to hold RGB values*/ 
typedef struct{
  cl_int r; 
  cl_int g;
  cl_int b; 
}color;


/*prints out the results */
void printResults(const cl_float *res, cl_int *col, cl_uint count); 
  

#define DATA_SIZE (1)
#define OUTPUT_SIZE (1024*1024)
int colorvals[3] = {255,255,0}; 
color map[16];

char *pid_file_path; 


/*writes a pid file*/
int writePID(int pid)
{
  FILE *pid_file; 
  char *file = (char*)malloc(sizeof(char)*100);
  char *value = (char*)malloc(sizeof(char)*100);  
  int size =0;  
  sprintf(file, "%s%d.pid", pid_file_path,pid);
  size = sprintf(value, "%d",pid); 
  pid_file=fopen(file,"w"); 
  if(pid_file == NULL)
    {
      
      return -1; 
    }
    fwrite(value,1,size,pid_file);
    fclose(pid_file);

 return 0; 
}


/*adds a center vector to the center list*/
void addCenterToList(cl_float *item)
{
  center=(cl_float*)realloc(center, sizeof(cl_float4)*4*(numobj+1));
  for(int i=0; i<4; i++)
    {
      center[(numobj*4)+i] = item[i]; 
    }
  
}


/*read a scene file
asumes that object are orded by their z values 
with the object with largest z value last in the file */ 
void readSceneFromFile(const char *filename)
{

   FILE *file;  

  file = fopen(filename,"r+"); // open in text mode
  
  if(file == NULL)
    {
      cout << "could not load file " << filename <<  endl; 

    }

 
  int res = 1; 

  while(res!=EOF)
    {
      float num; 
      
      res = fscanf(file, "%f", &num);
      if(res!=EOF)
	{
	  cl_float *centerval = (cl_float*)malloc(sizeof(cl_float)*4);
	  centerval[0]=num;
	
	  
	  for(int i=1; i<3; i++)
	    {
	  
	      
	      fscanf(file, "%f", &num); 
	

	      centerval[i] = num;
	  
	    }
	  float num2; 
	  float num3; 
	  centerval[3]=0; 
	  res=fscanf(file, "%f", &num2);

	  r=(cl_float*)realloc(r,sizeof(cl_float)*(numobj+1));

	  r[numobj] = num2;

	  res=fscanf(file, "%f", &num3); 
	  colorid=(cl_int*)realloc(colorid, sizeof(cl_int)*(numobj+1));
	  colorid[numobj]=(cl_int)int(num3);

	  addCenterToList(centerval); 
	  numobj++;
	}
    } 
}

/*assign a float4 array to another*/  
void float4Assign(cl_float *to, cl_float from[4])
{

  

 for(int i=0; i<4; i++)
    {
      to[i] = from[i];
    }
 
}


/*prints out a cl_float4
 for debugging purposes*/  
void printOutFloat4(cl_float in[4])
{
  for(int i=0; i<4;  i++)
    {
      cout << in[i] << endl; 

    }

}

/*creates a new Color with RGB values*/ 
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

/*read the scene from file if the filename is null use a static scene
NEED CLEAN UP*/ 
void readScene(const char *filename)
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


  float4Assign(light, inlight);
  float4Assign(origin, inorig); 


  if(filename!=NULL) /* if there is a scene file read settings else use static scene*/
    {
      readSceneFromFile(filename);
    }
  else 
    {
      
      cl_float *center1 =  (cl_float *)malloc(sizeof(cl_float)*4);
      /* 00-1
	 100
	 -0.500*/
      center1[0] = -1;
      center1[1] = 0.5;
      center1[2] = -1;
      center1[3] = 0; 
      cl_float *center2 = (cl_float *)malloc(sizeof(cl_float)*4);
      center2[0] = 1;
      center2[1] = 0.5;
      center2[2] = -1;
      center2[3] = 0;
      cl_float *center3 = (cl_float *)malloc(sizeof(cl_float)*4);
      center3[0] = 0;
      center3[1] = 0;
      center3[2] = 1;
      center3[3] = 0;

  

    
  
      addCenterToList(center1); 
      r[numobj] = 0.5;
      colorid[numobj] = 1;
      numobj++;
      addCenterToList(center2); 
      r[numobj] = 0.5; 
      colorid[numobj]=2; 
      numobj++; 
      addCenterToList(center3); 
      r[numobj] = 1.5; 
      colorid[numobj]=3; 
      numobj++;
    }
}


int main(int argc, char* argv[]){

/*set this to false if you don't want the file header to be written*/ 
bool write_header = true;
  

 if(argc < 5)
    {
      cout << "Usage: <path to OpenCL code> <startline> <stopline> <total lines> <scene_file>  <pid_file_path>" << endl; 
      exit(0);
    }

  /*filepath to scene file will be stored here*/ 
  char *filename = NULL;  


  /*optional arguments*/ 
 
  /*scenefile path */ 
  if(argc > 4) 
    {
      filename = argv[5];
      
      
    }
  
  /*if and where to write the PID file*/ 
  if(argc > 5)
    {
      if(realloc(pid_file_path,sizeof(char)*strlen(argv[6]))==NULL)
	{
	  cout << "failed to allocate memmory" << endl;  
	  exit(-1); 
	}

	pid_file_path = argv[6];

	int pid = getpid();
	if(writePID(pid)==-1)
	  {

	    cout << "failed to write pid " << endl; 
	    exit(-1); 

	  }
        

    }
  
  /*used to store error codes from OpenCL*/
  int err;
  int output_size = OUTPUT_SIZE;

  // OpenCL stuffs
  /*device identifier*/ 
  cl_device_id device_id;
  /*openCL context*/ 
  cl_context context;
  /*openCL command queue*/ 
  cl_command_queue queue;

  cl_program program;
  cl_kernel kernel;

  /*read in the width of the image to render*/ 
  /*this will be the number of times the OpenCL kernel will be run for each y*/ 
  cl_int count = atoi(argv[4]);
  cl_int pos =0;
 

  
  /*color id's*/
  /* should be in the scene file */

  map[0]=newColor(0,0,0);//black 
  map[1]=newColor(255,0,0);//red
  map[2]=newColor(0,255,0);//green 
  map[3]=newColor(0,0,255); //blue
  map[4]=newColor(255,255,0);//yellow
  map[5]=newColor(255,255,255);//white  
  map[6]=newColor(255,127,2); //orange  
  map[7]=newColor(222,0,255); //purple 
  map[8]=newColor(155,155,155); //gray 
  map[9]=newColor(2, 253,232); //turquoise
  map[10]=newColor(179,253,2); //slimegreen 
  map[11]=newColor(255,170,200); //pink
  

  
   
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
	 Load the program source 
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
     
      /*data[3] contians which y we are processing*/  
      cl_int data[3] = {y,0,atoi(argv[4])}; 
       cl_int start = atoi(argv[2]);
      cl_int stop  = atoi(argv[3]);

      /*write file header*/ 
      if(write_header)
	{
	  
	  cout << "P3\n" << count << " " << start-stop << "\n" << "255" << "\n";
        }
      
      /* read scene file if there was one provided 
	 if filename is null a default scene will be used*/ 
      readScene(filename);
            
           
      data[1] = numobj;
     
      /* for every y we calculate the pixel value of every x */
  for(y=start-1; y>=stop; y--)
    {

     
      /* color array will contian color id for each pixel */
      cl_int *colors = (cl_int*)malloc(count*sizeof(cl_int));

     
    
          data[0] = y;    
	  cl_float *results2 =  (cl_float*)malloc(count*sizeof(cl_float));
	
	  size_t global, local;


	  /*declare opencl memory objects*/ 
	  cl_mem input; 
	  cl_mem output;
	  cl_mem centermem;
	  cl_mem radiusmem;
	  cl_mem colorinmem;
	  cl_mem posmem;
	  cl_mem lightmem; 
	  cl_mem origmem; 
	  

      
	  
 

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
    
	
	  /**
	     Write stuff to the input buffer.
	  */
	
	  err = clEnqueueWriteBuffer(queue, input, CL_TRUE, 0, sizeof(cl_int) * 3,
				 &data, 0, NULL, NULL);

	  err  = clEnqueueWriteBuffer(queue, centermem, CL_TRUE, 0, sizeof(cl_float)*(4*numobj),
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

/* if the luminess value is larger then max return max else value. */ 
int getColorValue(int max, int value)
{
 
  
  if(value<=max)
      return value;
  else
      return max; 

}


/* prints out the results */

void printResults(const cl_float *res, cl_int *col,cl_uint count)
{

 
  for(cl_uint i=0;  i<count; i++)
    {
      int num = int(.5 + 255. * res[i]/16);
      
      
      if(num<0)
      {
        num = num * -1; 
      }
     
      
     

       color c = getColor(col[i]);
      
      
       int r = getColorValue(c.r,num); 
       int g = getColorValue(c.g,num); 
       int b = getColorValue(c.b,num);  
  
       cout << r << " " << g << " "  << b <<  "\n";
  
	

    }


}


