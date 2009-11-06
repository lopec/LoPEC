#define NULL ((void *)0)

//////////////////////////////////////////////////////////////////////////
typedef struct{
  float x;
  float y; 
  float z; 
}centervals;


/////////////////////////////////////////////////////////////////////////
typedef struct{
  int colid; 
  float val; 
}pixel;

//////////////////////////////////////////////////////////////////////////
typedef struct{
  int r;
  int g; 
  int b; 
}color; 


//////////////////////////////////////////////////////////////////////////

typedef struct{
  float r;
  float4 center;
  int color; 
}sphere; 

//////////////////////////////////////////////////////////////////////////

typedef struct{
  sphere val;
}element; 

//////////////////////////////////////////////////////////////////////////
/*typedef struct{
  sphere *value;
  sphere *next; 
}scene_element; 
*/

////////////////////////////////////////////////////////////////////////// 

typedef struct{
  float first; 
  float4 second;
  int  c;
}hits; 

//////////////////////////////////////////////////////////////////////////
/*typedef struct{
  scene_element *curr; 
  scene_element *head; 
  scene_element *it;
  int len;
  int itpos; 
  }scene;*/

typedef struct{
  sphere *list; 
  float4 light; 
  float4 origin; 
  int numobj; 

}scene; 

//////////////////////////////////////////////////////////////////////////

color newColor(int r, int g, int b)
{
  color c; 

  c.r = r; 
  c.g = g; 
  c.b = b; 

  return c; 
}

/////////////////////////////////////////////////////////////////////////

pixel newPixel(float val, int colid)
{
  pixel p; 

  p.val = val; 
  p.colid = colid; 

  return p; 
}


/////////////////////////////////////////////////////////////////////////
/*color getColorValues(float hitvalue, color c)
{
  hitvalue = (int)(.5 + 255. * hitvalue);
  
  if(hitvalue <=c.r)
    {
      c.r = hitvalue; 
    }

  if(hitvalue <=c.g)
    {

      c.g = hitvalue; 
    }

  if(hitvalue <= c.b)
    {
      c.b = hitvalue; 
    }

  return c; 

  }*/

//////////////////////////////////////////////////////////////////////////
 /*void createScene(scene *sc)
{
  sc->curr = NULL; 
  sc->head = NULL;
  sc->it = sc->head; 
  sc->itpos = 0; 
  sc->len = 0; 
 
  
  }*/

//////////////////////////////////////////////////////////////////////////
  /*void resetIterator(scene *sc)
{
  sc->it = sc->head; 
  sc->itpos = 0; 
  }*/


//////////////////////////////////////////////////////////////////////////
   /*void addToScene(sphere *s,scene *sc)
{
  scene_element tmp; 
  tmp.value = s; 
  tmp.next  = sc->head; 
  sc->curr = &tmp;
  sc->head = sc->curr;
  sc->len++; 

  }*/

void addToScene(sphere *s, scene *sc)
{
  //sphere sp; 
  //sp  = *s; 
  // sc->list[sc->numobj] =sp; 
  //sc->numobj++; 

}

//////////////////////////////////////////////////////////////////////////
    /*int sceneHasNext(scene *sc)
{
   return (sc->itpos < sc->len ? 1 : 0);
   }*/

/////////////////////////////////////////////////////////////////////////
     /*void getNextFromScene(scene *sc, sphere *s)
{
  
   scene_element *tmp; 

     
      tmp = sc->it;
      sc->it = tmp->next; 
      *s = *tmp->value;
      sc->itpos++; 
      }*/
//////////////////////////////////////////////////////////////////////////
void createHits(hits *hit,float first, float4 second)
{
   
  hit->first = first; 
  hit->second = second; 
    
}

////////////////////////////////////////////////////////////////////////

float4 unitise(float4 a)
{ 
  

  return (1 / sqrt(dot(a,a)) * a);

}




//////////////////////////////////////////////////////////////////////

void assign(float4 *f, float x, float y, float z)  
{
 
 *f = (float4)(x,y,z,0);
   

}

//////////////////////////////////////////////////////////////////////

float ray_sphere(float r, 
		 float4 center, 
		 float4 dir, 
		 float4 origin)
{
  float4 v;
 
  v =  center - origin;
  float b = dot(v,dir);
  float disc = b*b - dot(v,v) + r*r;
  if(disc < 0)return INFINITY;
  float d = sqrt(disc), t2 = b + d; 


  if(t2 < 0)return INFINITY;
  float t1 = b - d; 
  
  return (t1 > 0 ? t1 : t2); 


}

//////////////////////////////////////////////////////////////////////

void sphere_intersect(hits *hit,  
		      float4 d, 
		      float4 o, 
		      float4 center,
		      float r)
{

  float4 hitsecond;

  
  float lambda = ray_sphere(r,center,d,o);
  if(lambda < hit->first)
    {
      hit->first = lambda; 
      float4 tmp;
      tmp = normalize(o + ((lambda*d) -center));
      assign(&hitsecond,tmp[0],tmp[1],tmp[2]);
      hit->second = hitsecond;
      
    }


}

//////////////////////////////////////////////////////////////////////


float ray_trace(float4 light, 
		float4 o, 
		float4 d,
		float4 center,
		float r)
		
{
 
  hits hit;
  createHits(&hit,INFINITY,(float4)(0,0,0,0));
  float delta =1e-5f;
  
  sphere_intersect(&hit, d,o,center,r);
  
 
  if(hit.first == INFINITY)
    {
      
      return 0.;

    }  
  float g = dot(hit.second, light); 
  if(g >= 0) { return 0.;} 
  float4 p = o + ((hit.first * d)+(delta * hit.second)); 
 
  
  hits hit2; 
  createHits(&hit2, INFINITY, (float4)(0,0,0,0));
  

  float4 dir2 = (float4)-1.*light;
  //sphere sp = s; 
  sphere_intersect(&hit2,dir2,p,center, r);
 

  return (hit2.first < INFINITY ? 0 : -g); 
 

}


////////////////////////////////////////////////////////////////////
/*start the ray tracing for each object in the scene
  WARNING THIS IS UGLY !
 */

/*float trace(float4 light,
	    float4 o,
	    float4 d,
	    scene *s,
	    int *color)
{
  float highest=0;
  float sz = -100;  
  resetIterator(s); 
  while(sceneHasNext(s))
    {
      sphere sp; 
      getNextFromScene(s, &sp);
      
      float f = ray_trace(light, o,d,sp);
      float4 pos = sp.center; 
      float z = pos[2]; 
     
       if(f > 0)
       {

	 if(sz<z)
	 { 
	       sz =z; 
	       highest = f;  
	       int c = sp.color; 
	       *color = c;
	   
	 }
	 else if(sz==z)
	   {
	     if(highest <= f)
	       {
		 highest = f; 
		 int c = sp.color; 
		 *color =c; 
	       
	       }
	   
	   }
	  
	 
	}	
     }
  
  return highest; 


  }*/


///////////////////////////////////////////////////////////////////

__kernel void hello(__global int *input,
		    __global float *centerin,
		    __global float  *r,
		    __global int *colorin,
		    __global float *lightin,
		    __global float * origin, 
		    __global float  *output, 
		    const unsigned int count,
		    __global int *color)
		    
{
  int x = get_global_id(0); 

  int start = input[0]; 
  int stop =  input[1];
  int pixels = input[2];
   

  int y = input[0]; 
  int n = count; 
  int d = 0; 
  sphere s;
  sphere s2; 
  sphere s3; 
  scene  sc;

  
  
  
 
  
  float g = 0.0;
  
  float4 ul;
  assign(&ul, 0, -3,2);
  float4 light;
  light = normalize(ul);
  assign(&light, light[0],light[1],light[2]);
  float4 orig;
  assign(&orig,0,0,-4);
  float4 dir;
    
  int colid=0; 

  float4 dist;
  assign(&dist, x-n/2., y-n/2., n);	  
  dir = normalize(dist);
  assign(&dir, dir[0],dir[1],dir[2]);
  
	  //  g = 16*trace(light,orig,dir,center,r,colorin,input[1],&colid);
	
	  
	  
	  // g = 16*ray_trace(light,orig,dir,sin[0]);
	  
	 int i = 0; 
  int numobj = input[1]; 
  
  float highest = 0; 
  for(int i=0; i<numobj; i++)
   {
     
      float4 in;
      assign(&in, centerin[(i*4)], centerin[(i*4)+1],centerin[(i*4)+2]);
       
       float f;
       f = 16*ray_trace(light, orig,dir,in, r[i]);
    
        if(f > 0)
       {
	       highest = f;  
	       colid = colorin[i];

	 
	       }

 
    
       }
  
  
  
 
   
   
    
 
  
	  
   output[x] = highest;       
    color[x] = colid;
      //pos2++;
  

 
  


}
