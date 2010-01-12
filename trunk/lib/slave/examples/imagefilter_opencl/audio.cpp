/*sharpen.cpp 
  Author:Christofer Ferm 
  LoPEC Low Power Erlang-based Cluster
*/
#include "sharpen.h"

#define AUDIO_PATH "sharpen.cl"

using namespace std;

cl_device_id device_id; 
cl_context context; 
cl_command_queue queue; 
cl_program program; 
cl_kernel kernel;
cl_int count; 
const char *KernelSource; 
color *image; 
color *result; 
cl_int image_width; 
cl_int image_height; 


int audio(char *filename)
{
  


  if(read_OpenCL((char*)SHARPEN_PATH,(char**)&KernelSource)==-1)
    {
      cout << "failed to read OpenCL source file:"<< SHARPEN_PATH << endl; 
      return -1; 
    }

  read_header(filename,&image_width, &image_height); 
  
  
  image=(color*)realloc(image, sizeof(color*)*(image_width*image_height)*3);
  result=(color*)realloc(image, sizeof(color*)*(image_width*image_height)*3);
  if(read_ppm(filename,image,&image_width, &image_height)==-1)
    {
      cout << "could not read file " << filename << endl; 
      return -1; 

    }

  

  init_OpenCL(); 
  // printHeader(image_width, image_height);
  /* cl_float *test = (cl_float*)malloc(sizeof(cl_float)*(image_width*image_height)*3); 
     test = color2float(image,test,image_width,image_height);
     outputImage_float(test,image_width,image_height); */

   do_calc_OpenCL(); 

 
   //  outputImage(image, image_width, image_height); 
  

  return 0;

}

int init_OpenCL()
{

  int err = 0;

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
	return -1;
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
	return -1;
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
	return -1;
      }

      /**
	 Load the program source 
      */
      

      program = clCreateProgramWithSource(context, 1, &KernelSource, NULL, &err);

      if (err != CL_SUCCESS){
	cout << "clCreateProgramWithSource :( %d" << err << endl;
	return -1;
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
	return -1;
      }

      /**
	 Instansiate the kernel.
      */
      kernel = clCreateKernel(program, "sharpen_filter", &err);

      if (err != CL_SUCCESS){
	cout << "clCreateKernel :( " << err << endl;
	return -1;
      }

      return 0;

}


int do_calc_OpenCL()
{
  
  cl_float *datar = (cl_float*)malloc((sizeof(cl_float)*(image_width*image_height)));
  cl_float *datag = (cl_float*)malloc((sizeof(cl_float)*(image_width*image_height)));
  cl_float *datab = (cl_float*)malloc((sizeof(cl_float)*(image_width*image_height)));
  cl_float *resultsr = (cl_float*)malloc((sizeof(cl_float)*(image_height)));
  cl_float *resultsg = (cl_float*)malloc((sizeof(cl_float)*(image_height)));
  cl_float *resultsb = (cl_float*)malloc((sizeof(cl_float)*(image_height)));
  color2float(image,datar,datag,datab,image_width,image_height);
  cl_mem inputr;
  cl_mem inputg; 
  cl_mem inputb; 
  cl_mem input; 


  const size_t origin[3] = {0,0,0};
  const size_t region[3] = {image_width, image_height, 1};
  
  cl_int *image2=(cl_int*)malloc(sizeof(int)*(image_width*image_height)*4);
  
  image2 = color2intAlpha(image, image2, image_width, image_height);
  
   
  cl_int err = 0; 
  /*create input buffers */

  cl_image_format image_format; 
  image_format.image_channel_order = CL_ARGB; 
  image_format.image_channel_data_type = CL_UNSIGNED_INT8;

   input = clCreateImage2D(context, CL_MEM_READ_ONLY,&image_format,image_width, image_height,0,NULL, &err); 
  
  
  if (err != CL_SUCCESS){
    cout << "clCreateImage2Dinput: " << err << endl;
    exit(1);
    }

 
 

  inputr = clCreateBuffer(context, CL_MEM_READ_WRITE,
			  sizeof(cl_float)*(image_width*image_height) , NULL, &err);
  if (err != CL_SUCCESS){
    cout << "clCreateBufferinput: " << err << endl;
    exit(1);
  }

  inputg = clCreateBuffer(context, CL_MEM_READ_WRITE,
			  sizeof(cl_float)*(image_width*image_height) , NULL, &err);
  if (err != CL_SUCCESS){
    cout << "clCreateBufferinput: " << err << endl;
    exit(1);
  }
  
  inputb = clCreateBuffer(context, CL_MEM_READ_WRITE,
			  sizeof(cl_float)*(image_width*image_height) , NULL, &err);
  if (err != CL_SUCCESS){
    cout << "clCreateBufferinput: " << err << endl;
    exit(1);
  }


    /**
       Write stuff to the input buffer.
    */
  
   err = clEnqueueWriteImage(queue, input, CL_TRUE, origin, region, image_width*sizeof(cl_int)*4,0,image2,0,NULL,NULL); 

  if(err !=CL_SUCCESS)
    {
      cout << "clEnqueueReadImage err:" << err << endl;
      exit(0);
      }

  

  err = clEnqueueWriteBuffer(queue, inputr, CL_TRUE, 0, sizeof(cl_float)*(image_width*image_height),
			     datar, 0, NULL, NULL);
  
  err = clEnqueueWriteBuffer(queue, inputg, CL_TRUE, 0, sizeof(cl_float)*(image_width*image_height),
			     datag, 0, NULL, NULL);
  
  err = clEnqueueWriteBuffer(queue, inputb, CL_TRUE, 0, sizeof(cl_float)*(image_width*image_height),
			     datab, 0, NULL, NULL);
	  

  cl_int x = 0;
  
  for(x=1; x<=image_width-1; x++)
  {

        size_t global, local;


	 /*declare opencl memory objects*/ 
       
	 cl_mem outputr;
	 cl_mem outputg; 
	 cl_mem outputb;
	 cl_int y = image_height-1; 
	  
	 
      
	  
 

	  /**
	     Create buffers
	  */
	 
	
	  	   

	  outputr = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR,
				  sizeof(cl_float)*(image_height), NULL, &err);
	  if (err != CL_SUCCESS){
	    cout << "clCreateBufferoutput: " << err << endl;
	    exit(1);
	  }
	  
	   outputg = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR,
				    sizeof(cl_float)*(image_height), NULL, &err);
	  if (err != CL_SUCCESS){
	    cout << "clCreateBufferoutput: " << err << endl;
	    exit(1);
	  }

	   outputb = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR,
				  sizeof(cl_float)*(image_height), NULL, &err);
	  if (err != CL_SUCCESS){
	    cout << "clCreateBufferoutput: " << err << endl;
	    exit(1);
	  }

	  //cl_mem output = clCreateImage2D(context, CL_MEM_READ_WRITE,&image_format,image_width, image_height,0,NULL, &err); 
	  /*cl_mem  output = clCreateBuffer(context, CL_MEM_READ_WRITE,
				  sizeof(cl_int)*(image_height*image_width)*4, NULL, &err);
  
	   if (err != CL_SUCCESS){
	     cout << "clCreateImage2Dinput: " << err << endl;
	     exit(1);
	     }*/
	  
	 
	
	
	  /*
	    Set kernel arguments
	  */
	  clSetKernelArg(kernel, 0, sizeof(cl_mem), &input);
	   //clSetKernelArg(kernel, 0, sizeof(cl_mem), &output);
	  clSetKernelArg(kernel, 1, sizeof(cl_mem), &inputr);
	  clSetKernelArg(kernel, 2, sizeof(cl_mem), &inputg); 
	  clSetKernelArg(kernel, 3, sizeof(cl_mem), &inputb); 
	  clSetKernelArg(kernel, 4, sizeof(cl_mem), &outputr);
	  clSetKernelArg(kernel, 5, sizeof(cl_mem), &outputg);
	  clSetKernelArg(kernel, 6, sizeof(cl_mem), &outputb);
	  clSetKernelArg(kernel, 7, sizeof(cl_int), &x);
	  clSetKernelArg(kernel, 8, sizeof(cl_int), &image_width);
	  clSetKernelArg(kernel, 9, sizeof(cl_int), &image_height);
	 
	  


	  local = 1;

 
 
	  /**
	     Execute the kernel over the entire range of the data set.
	  */


	  global = y;
	  err = clEnqueueNDRangeKernel(queue, kernel, 1, NULL , &global, &local,
			 0, NULL, NULL);
  
	  if(err != CL_SUCCESS)
	    {
	      cout << "could not execute Kernel Error Code:" << err << endl;  
	      exit(1);
	    }



 
	  /**
	     Wait for the command queue to get serviced before reading back results
	  */
	  clFinish(queue);
  
	  /**
	     Read the results from the device
	  */

	  

	   err = clEnqueueReadBuffer(queue, outputr, CL_TRUE, 0,
				    sizeof(cl_float)*(image_height), resultsr , 0, NULL, NULL );
	  
	  if (err != CL_SUCCESS){
	    cout << "clEnqueueReadBuffer1: " << err << endl;
	    exit(1);
	    }

	  err = clEnqueueReadBuffer(queue, outputg, CL_TRUE, 0,
				    sizeof(cl_float)*(image_height), resultsg , 0, NULL, NULL );
	  
	  if (err != CL_SUCCESS){
	    cout << "clEnqueueReadBuffer1: " << err << endl;
	    exit(1);
	  }

	  err = clEnqueueReadBuffer(queue, outputb, CL_TRUE, 0,
				    sizeof(cl_float)*(image_height), resultsb , 0, NULL, NULL );
	  
	  if (err != CL_SUCCESS){
	    cout << "clEnqueueReadBuffer1: " << err << endl;
	    exit(1);
	  }

	 

	  /*err = clEnqueueReadImage(queue, output, CL_TRUE, origin, region, 
	    image_width*sizeof(cl_int)*4,0, ir,0,NULL, NULL);*/

	  /*err = clEnqueueReadBuffer(queue, output, CL_TRUE, 0,
				    sizeof(cl_int)*(image_height*image_width)*4, ir , 0, NULL, NULL );
	  
	  if (err != CL_SUCCESS){
	    cout << "clEnqueueReadImage1: " << err << endl;
	    exit(1);
	    }*/
       	  
			  
 
	  result=putPixels(result,resultsr,resultsg,resultsb,x,image_width, image_height);
	  
	  //result = intAlpha2Color(ir, result, image_width, image_height);

	  //clean up
	
	  //clReleaseMemObject(output);
	  clReleaseMemObject(outputr);
	  clReleaseMemObject(outputg);
	  clReleaseMemObject(outputb);
	  
           
	  
  }

  outputImage(result, image_width, image_height);

  // Shut down and clean up
  //clReleaseMemObject(input);
    clReleaseMemObject(inputr);
    clReleaseMemObject(inputg);
    clReleaseMemObject(inputb);
    clReleaseContext(context);
    clReleaseProgram(program);
    clReleaseCommandQueue(queue);
    clReleaseKernel(kernel);
    free(result);

}
