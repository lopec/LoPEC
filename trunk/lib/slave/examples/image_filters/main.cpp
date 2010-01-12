/* LoPEC 
   Low Power Erlang-based Cluster
   Author: Christofer Ferm and Henrik Thalin*/ 

#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define sharp_w 3
#define sharp_h 3
#define emboss_h 3
#define emboss_w 3
#define gauss_w 7 
#define gauss_h 7
#define gauss_width 7  

typedef struct
{ 
  int r; 
  int g; 
  int b; 
}color; 

color *image1;  
color *filtered_image;
color *emboss_image; 
int image_height; 
int image_width; 
 

using namespace std;  

/* defining the filter matrixes*/ 
int sumr=0;
int sumg=0; 
int sumb=0;
int emboss_filter[emboss_w][emboss_h]={{2,0,0},{0,-1,0},{0,0,-1}};
int emboss_sum=1; 
int sharpen_filter[sharp_w][sharp_h]={{0,-1,0},{-1,5,-1},{0,-1,0}};
/*int sharpen_filter[sharp_w][sharp_h]={{-2,-2,-2,-2,-2},{-2,-4,-4,-4,-2},{-2,-4,65,-4,-2},{-2,-4,-4,-4,-2},{-2,-2,-2,-2,-2}};
 */
//int sharpen_filter[sharp_w][sharp_h]={{-1,-1,-1,-1,-1},{-1,-2,-2,-2,-1},{-1,-2,33,-2,-1},{-1,-2,-2,-2,-1},{-1,-1,-1,-1,-1}};


int sharp_sum=1; 
int gauss_fact[gauss_width]={1,6,15,20,15,6,1};
int gauss_filter[gauss_w][gauss_h] = {{1,6,15,20,15,6,1},{6,36,90,120,90 ,36 ,6},{15,90,225,300,225,90 ,15},{20,120,300,400,300,120,20},{15,90,225,300,225,90 ,15},{6,36,90,120,90 ,36 ,6},{1,6,15,20,15,6,1}}; 
int gauss_sum=4096;




void put_pixel(int x, int y, color c, color *image)
{
  
  image[y*image_width+x]=c; 

}


color createColor(int r, int g, int b)
{
  color c; 

  if(r > 255)
    {
      r = 255; 
    }
  if(g > 255)
    {
      g = 255; 
    }
  if(b > 255)
    {
      b = 255;
    }

  if(r < 0)
    {
      r = 0; 
    }

  if(g < 0)
    {
      g = 0; 
    }
  if(b < 0)
    {
      b = 0;

    }
  c.r = r; 
  c.g = g; 
  c.b = b;

  return c; 

}


/*TODO */

void read_ppm(char *filename)
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

  image_width = width; 
  image_height = height; 

 
  image1 = (color*)realloc(image1, sizeof(color*)*((width*height)*3));
  filtered_image = (color*)realloc(filtered_image, sizeof(color*)*((width*height)*3)); 
  emboss_image = (color*)realloc(emboss_image, sizeof(color*)*((width*height)*3));
  int count = 0; 
  while(res!=EOF)
    {
      color tmp;
      
      fscanf(file, "%d", &tmp.r);
      fscanf(file, "%d", &tmp.g);
      res=fscanf(file, "%d", &tmp.b);

      image1[count] = tmp; 
      count++; 
      
    } 




}


void printImage(color *image, int width, int height) 
{
  cout << "P3\n" << width << " " << height << endl; 
  
  int limit= width*height; 
  for(int i=0; i<limit; i++)
    {
      color tmp = image[i];
      
      cout << tmp.r << " " << tmp.g << " " << tmp.b << endl; 

    }
  


}

/*TODO*/ 
color getPixel(int x, int y, color *image)
{
  
  

  /*  if(x <0 || y<0 || x>image_width || y>image_height)
    {
      return res;

      }*/

  color res = image[y*image_width+x]; 

  return res; 
}

void test(int x, int y)
{



}

void apply_filter(int x, int y)
{ 

  sumr = 0; 
  sumg = 0; 
  sumb = 0;

  if(x == 0 || y == 0 || x == image_width || y == image_height)
    {
      

      put_pixel(x,y,getPixel(x,y,image1),filtered_image); 

    }
  else
    {

      for(int k=0;k<sharp_w;k++)
	for(int l=0;l<sharp_h;l++)
	  {
	    color col=getPixel(x-((sharp_w-1)>>1)+k,y-((sharp_h-1)>>1)+l,image1);
	
	    int r = col.r;
	    int g = col.g;
	    int b = col.b; 
	
	    sumr+=r*sharpen_filter[k][l]; 
	    sumg+=g*sharpen_filter[k][l]; 
	    sumb+=b*sharpen_filter[k][l]; 

	

	  }

  
      sumr/=sharp_sum;
      sumg/=sharp_sum; 
      sumb/=sharp_sum; 


      put_pixel(x,y,createColor(sumr, sumg, sumb),filtered_image); 
    }
}


void grayscale(int x, int y)
{

  color col=getPixel(x,y,image1);
	
	int r = col.r;
	int g = col.g;
	int b = col.b; 
	
	int h = (r+g+b)/3;

	
  
	put_pixel(x,y,createColor(h, h, h), filtered_image); 


}


void emboss()
{

   for(int i=0;i<image_width;i++){
     for(int j=0;j<image_height;j++){
       sumr=0;
       for(int k=0;k<emboss_w;k++){
	 for(int l=0;l<emboss_h;l++){
	   color col=getPixel(i-((emboss_w-1)>>1)+k,j-((emboss_h-1)>>1)+l,filtered_image);
	   int r=col.r;
	   sumr+=r*emboss_filter[k][l];
	 }
       }
       

       if(i == 0 || j == 0 || i == image_width || j == image_height)
	 {
      
	   put_pixel(i,j,getPixel(i,j,filtered_image),emboss_image); 

	 }
       else
	 {
	   sumr/=emboss_sum;
	   sumr+=128;
       
	   put_pixel(i,j,createColor(sumr,sumr,sumr),emboss_image);
	 }

     }
   

   }
  


}



void gauss_blur2(int x, int y)
{

  sumr = 0; 
  sumg = 0; 
  sumb = 0;

  if(x < 4 || y < 4 || x > (image_width-4) || y > (image_height-4))
    {
      

      put_pixel(x,y,getPixel(x,y,image1),filtered_image); 

    }
  else
    {

      for(int k=0;k<gauss_w;k++)
	for(int l=0;l<gauss_h;l++)
	  {
	    color col=getPixel(x-((gauss_w-1)>>1)+k,y-((gauss_h-1)>>1)+l,image1);
	
	    int r = col.r;
	    int g = col.g;
	    int b = col.b; 
	
	    sumr+=r*gauss_filter[k][l]; 
	    sumg+=g*gauss_filter[k][l]; 
	    sumb+=b*gauss_filter[k][l]; 

	

	  }

  
      sumr/=gauss_sum;
      sumg/=gauss_sum; 
      sumb/=gauss_sum; 


      put_pixel(x,y,createColor(sumr, sumg, sumb),filtered_image); 
    }



}

void gauss_blur()
{


 for(int i=0;i<image_width;i++){
  for(int j=0;j<image_height;j++){
    if(i == 0 || j == 0 || i == image_width || j == image_height)
	 {
      
	   put_pixel(i,j,getPixel(i,j,image1),filtered_image); 

	 }
    else
      {
	sumr=0;
	sumg=0;
	sumb=0;
	for(int k=0;k<gauss_width;k++){
    
	  color col=getPixel(i-((gauss_width-1)>>1)+k,j, image1);
	  int r= col.r;
	  int g= col.b;
	  int b= col.g;
	  sumr+=r*gauss_fact[k];
	  sumg+=g*gauss_fact[k];
	  sumb+=b*gauss_fact[k];

	}


	put_pixel(i,j,createColor(sumr/gauss_sum,sumg/gauss_sum,
			sumb/gauss_sum),filtered_image);
	
      } 
  }
 }

 
 
for(int i=0;i<image_width;i++){
  for(int j=0;j<image_height;j++){
     if(i == 0 || j == 0 || i == image_width || j == image_height)
       {
	   
	 color c = getPixel(i,j,filtered_image); 
	   put_pixel(i,j,c,emboss_image); 
	    
	   //cout << "writing from original  " << c.r << " " << c.g << " " << c.b << endl;

	 }
     else
       {
	 sumr=0;
	 sumg=0;
	 sumb=0;
	 for(int k=0;k<gauss_width;k++){
	   color col=getPixel(i,j-((gauss_width-1)>>1)+k,filtered_image);
	   int r= col.r;
	   int g= col.g;
	   int b= col.b;
     
	   sumr+=r*gauss_fact[k];
	   sumg+=g*gauss_fact[k];
	   sumb+=b*gauss_fact[k];
	   
	 }

 
	 
	 sumr/=gauss_sum;
	 sumg/=gauss_sum;
	 sumb/=gauss_sum;
    
	 //cout << "writing color  " << sumr << " " << sumg << " " << sumb << endl;  
	 put_pixel(i,j,createColor(sumr,sumg,sumb),emboss_image);
 
       }
  }
 } 





}



void twin(int x, int y)
{

  color col = getPixel(x,y,image1);

  //put_pixel(x,y,col); 

  cout << col.r << " " << col.g << " " << col.b << endl;

}

int main(int argc, char *argv[])
{

  if(argc < 2)
    {
      cout << "usage: " << argv[0] << " <filter type| (s)harpness | (b)lur | (g)rayscale | (e)mboss>" << "<input.ppm>" << endl;  

      exit(0); 
    }


  if(strcmp(argv[1],"s")==0)
	{
		  
	   read_ppm(argv[2]); 
   
	   for(int i=0;i<image_width;i++)
	     for(int j=0;j<image_height;j++)
	       apply_filter(i,j);
	   printImage(filtered_image, image_width, image_height);
	}
      else if(strcmp(argv[1],"b")==0)
	{

	   read_ppm(argv[2]); 
	   for(int i=0;i<image_width;i++)
	     for(int j=0;j<image_height;j++)
	       gauss_blur2(i,j);
	 printImage(filtered_image, image_width, image_height);

	}
       else if(strcmp(argv[1],"g")==0)
	 {
	   read_ppm(argv[2]); 
	   for(int i=0;i<image_width;i++)
	     for(int j=0;j<image_height;j++)
	       grayscale(i,j);

	   printImage(filtered_image, image_width, image_height);

	 }
       else if(strcmp(argv[1],"e")==0)
	 {
	   read_ppm(argv[2]);
	   for(int i=0;i<image_width;i++)
	     for(int j=0;j<image_height;j++)
	       grayscale(i,j);
	   
	   emboss();
	   printImage(emboss_image, image_width, image_height);
	   
	 }
       else
	 {
	   cout << "invalid filter " << argv[1] << endl;
  	   cout << "usage:filter <type|(s)harpen (b)lur (g)rayscale (e)mboss> <input_image_filename>" << endl;
	   exit(0);
	 }

 
    
  // emboss();
  
  // gauss_blur2();

  

  return 0; 
}

