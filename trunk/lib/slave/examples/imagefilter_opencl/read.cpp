/*read.cpp
  Author:Christofer Ferm 
  LoPEC Low Power Erlang-based Cluster*/ 

#include "read.h" 

using namespace std;

/*reads the header of a PPM file*/ 
int read_header(char *filename, int *image_width, int *image_height)
{
  
  FILE *file;  
  file = fopen(filename,"r+"); // open in text mode
  
  if(file == NULL)
    {
      cout << "could not load file " << filename <<  endl; 

    }

 
  int res = 1; 

  int width=0; 
  int height=0;  

  if(res!=EOF)
    {
      char s[2];  
      fscanf(file, "%s", s); 
      fscanf(file, "%d", &width); 
      fscanf(file, "%d", &height); 

    }

  *image_width  = width; 
  *image_height = height; 
  fclose(file); 

}

/*reads a ppm image file
 returns 0 on success and -1 on failure*/ 
int read_ppm(char *filename, color *image, int *image_width, int *image_height)
{

  FILE *file;  
  file = fopen(filename,"r+"); // open in text mode
  
  if(file == NULL)
    {
      cout << "could not load file " << filename <<  endl; 
      return -1; 
    }

 
  int res = 1; 

  int width=0; 
  int height=0;  

  if(res!=EOF)
    {
      char s[2];  
      fscanf(file, "%s", s); 
      fscanf(file, "%d", &width); 
      fscanf(file, "%d", &height); 

    }

  *image_width = width; 
  *image_height = height; 
 
   
 
  int count = 0; 
  while(res!=EOF)
    {
      color tmp;
      
      fscanf(file, "%d", &tmp.r);
      fscanf(file, "%d", &tmp.g);
      res=fscanf(file, "%d", &tmp.b);

      image[count] = tmp; 
      count++; 
      
    } 

  fclose(file);
  
  return 0;

}

/*reads OpenCL code form file
 returns 0 on success and -1 on failure*/ 

int read_OpenCL(char *filename, char **KernelSource) 
{
  FILE *file;  

  file = fopen(filename,"r"); // open in text mode
  
  if(file == NULL)
    {
      cout << "could not load file " << filename <<  endl; 

      return -1; 
    }

  fseek(file, 0L, SEEK_END); 
  long filelen = ftell(file); 
  rewind(file); 
  
  *KernelSource = (char*)calloc(filelen + 1, sizeof(char));  
  
  if(KernelSource == NULL)
    {
      cout << "insufficient memory to read file into memory" << endl; 
      return -1;
    }
     
  size_t readlength;
  
  readlength = fread((void*)*KernelSource,filelen, 1, file); 

  
   
  return 0; 

}


