/*Author:Christofer Ferm 
  a simple sharpen filter kernel  
*/

/*Defines for the filter size*/
#define sharp_w 3 
#define sharp_h 3 

typedef struct{
  int r; 
  int g; 
  int b; 
}color; 

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
float4 apply_filter(float r, float g, float b, float filter) 
{

  float4 res = {r*filter,g*filter,b*filter,0};  

  return res; 
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*the kernel containing the code to be run*/
__kernel void sharpen_filter(__read_only image2d_t input,
			     __global float *outputr,
			     __global float *outputg,
			     __global float *outputb,
			     __global int x)
{

     
  sampler_t sampler = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_NONE | CLK_FILTER_LINEAR; 
  
  int2 coord = (int2)(0,0); 
  int4 value = (int4)(0,0,0,0); 

  int filter[sharp_w][sharp_h]={{0,-1,0},{-1,5,-1},{0,-1,0}};
   int sharp_sum = 1;

   int height = get_image_height(input);
   int width = get_image_width(input);
   int y = height-get_global_id(0); 
   
  
  
      float sumr = 0.0; 
      float sumg = 0.0; 
      float sumb = 0.0;

      float4 result = {0,0,0,0}; 
         

      int tmpx = x-((sharp_w-1)>>1); 
      int tmpy = y-((sharp_h-1)>>1); 
      
      
      coord = (int2)(tmpx,tmpy);
      
      value = read_imagei(input, sampler, coord);
      
      float r = value[0];
      float g = value[1]; 
      float b = value[2];
      
      result+= apply_filter(r, g, b, filter[0][0]);
      
       
      coord = (int2)(tmpx,tmpy+1);

      value = read_imagei(input, sampler, coord);
      
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[0][1]);
	    
     

	    
      coord = (int2)(tmpx,tmpy+2);

      value = read_imagei(input, sampler, coord);
      
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[0][2]);
	
	    
      	    
            
      coord = (int2)(tmpx+1,tmpy);
      
      value = read_imagei(input, sampler, coord);
      
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[1][0]);
	
        	    
      	      
      coord = (int2)(tmpx+1,tmpy+1);

      value = read_imagei(input, sampler, coord);
      
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[1][1]);
	
             
	     
      coord = (int2)(tmpx+1,tmpy+2);

      value = read_imagei(input, sampler, coord);
     
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[1][2]);
	      
      
      coord = (int2)(tmpx+2,tmpy);
	     
      value = read_imagei(input, sampler, coord);
      
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[2][0]);
	
      
      coord = (int2)(tmpx+2,tmpy+1);

      value = read_imagei(input, sampler, coord);
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[2][1]);
	
      

     	     
      coord = (int2)(tmpx+2,tmpy+2);
	
      value = read_imagei(input, sampler, coord);
      r = value[0];
      g = value[1]; 
      b = value[2];
    
      result+= apply_filter(r, g, b, filter[2][2]);
	
           
      outputr[y] = result[0]; 
      outputg[y] = result[1];
      outputb[y] = result[2]; 
   
}
