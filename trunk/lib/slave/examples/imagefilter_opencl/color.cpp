/*color.cpp 
  Author:Christofer Ferm 
  LoPEC Low Power Erlang-based Cluster*/ 


#include "main.h"

using namespace std;

/* prints image to stdout */ 
int outputImage(color *image, int width , int height)
{ 
  cout << "P3\n" << width << " " << height << endl; 
  
  int limit= width*height; 
  for(int i=0; i<limit; i++)
    {
      color tmp = image[i];
      
      cout << tmp.r << " " << tmp.g << " " << tmp.b << endl; 

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

color createColor(cl_float r, cl_float g, cl_float b)
{
  color c; 

  if(r > 255.)
    {
      r = 255.; 
    }
  if(g > 255.)
    {
      g = 255.; 
    }
  if(b > 255.)
    {
      b = 255.;
    }

  if(r < 0.)
    {
      r = 0.; 
    }

  if(g < 0.)
    {
      g = 0.; 
    }
  if(b < 0.)
    {
      b = 0.;

    }
  c.r = r; 
  c.g = g; 
  c.b = b;

  return c; 

}


/*convert a color array into a float array*/
void color2float(color *in, cl_float *outr, cl_float *outg, cl_float *outb, int width, int height)
{

  
  for(int i=0; i<width*height; i++)
    {
      
      outr[i] = in[i].r;
      outg[i] = in[i].g; 
      outb[i] = in[i].b; 
      
    }

}

color *float2color(cl_float *in, color *out, int width, int height)
{

  int j=0; 
  for(int i=0; i<width*height; i++)
    {
      
      out[i].r = in[j];
      j++;
      out[i].g = in[j]; 
      j++;
      out[i].b = in[j]; 
      j++; 
    }
  return out; 

}

/* prints image to stdout */ 
int outputImage_float(cl_float *r, cl_float *g, cl_float *b, int width , int height)
{ 
 
 
  for(int i=1; i<=width-1; i++)
    {
      

      color c = createColor(r[i],g[i],b[i]);
      cout << c.r << " " << c.g << " " << c.b << endl;
     

    }
  


}

int printHeader(int width, int height)
{

   cout << "P3\n" << width << " " << height << endl; 
   return 0;
}

color *putPixels(color *res, cl_float *r, cl_float *g, cl_float *b, int x, int width, int height)
{
 
  for(int y=1; y<height-1; y++)
    {  

      color c = createColor(r[y],g[y],b[y]);
      res[y*width+x] = c; 
    }

  return res; 

}


int *color2intAlpha(color *in, int *out, int width, int height)
{
  int j=0;
  for(int i=0; i<width*height*4; i++)
    {
      out[i] = 1; 
      i++; 
      out[i] = in[j].r;
      i++; 
      out[i] = in[j].g; 
      i++;
      out[i] = in[j].b;
      


      j++; 
    }


  return out; 


}

color *intAlpha2Color(int *in, color *out, int width, int height)
{

    int j=0;
  for(int i=0; i<width*height*4; i++)
    {

      
      i++; 
      out[j].r = in[i];
      i++; 
      out[j].g = in[i]; 
      i++;
      out[j].b = in[i];
      


      j++; 
    }


  return out;



}
