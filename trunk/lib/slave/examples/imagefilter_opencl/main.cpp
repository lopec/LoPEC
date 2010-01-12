/*
main.c 
Author:Christofer Ferm
An Image/Audio filter application for 
LoPEC Low Power Erlang-based Cluster 
*/
#include "sharpen.h"


using namespace std;


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


int main(int argc, char* argv[]){

 
  if(argc < 2)
    {

      cout << "usage:filter <type|(s)harpen (b)lur (g)rayscale (e)mboss> <input_image_filename>" << endl;  
      exit(0); 
    }
  else 
    {
      
      if(strcmp(argv[1],"s")==0)
	{
	  sharpen(argv[2]); 
	  
	  //  cout << "sharpen" << argv[2] << endl; 

	  
	}
      else if(strcmp(argv[1],"b")==0)
	{
	  //blur(argv[2]);
	  cout << "blur" << argv[2] << endl;

	}
       else if(strcmp(argv[1],"g")==0)
	 {
	   cout << "grayscale" << argv[2] << endl;
	   //grayscale(argv[2]); 
	 }
       else if(strcmp(argv[1],"e")==0)
	 {
	   cout << "emboss" << argv[2] << endl;
	   //emboss(argv[2]); 
	   
	 }
       else if(strcmp(argv[1],"a")==0)
	 {
	   

	 }
       else
	 {
	   cout << "invalid filter " << argv[1] << endl;
  	   cout << "usage:filter <type|(s)harpen (b)lur (g)rayscale (e)mboss> <input_image_filename>" << endl;
	   exit(0);
	 }

    }

   
   
  
  return 0;
}


