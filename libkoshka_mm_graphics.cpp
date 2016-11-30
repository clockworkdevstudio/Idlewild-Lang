/*

Copyright (c) 2014-2016, Clockwork Dev Studio
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

extern "C"
{
    void bb_fatal_error(char *msg);
}

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <GL/glew.h>
#include <GL/glext.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>
#define GLM_FORCE_RADIANS
#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/transform.hpp>
#include <stdio.h>
#include <math.h>
#include <string>
#include "libkoshka_mm.h"
#include <glib.h>

extern "C"
{
    void bb_init_libkoshka_core(unsigned long long int max_gosub_depth);
    void bb_final_libkoshka_core();
    void load_primitive_program();
    void load_image_program();
    GLuint load_shader(GLenum type, const GLchar **shaderCode,int num_lines);
    unsigned long long int mask_surface(SDL_Surface *surface,GLuint mask_color,GLuint prev_mask_color);
    unsigned long long int **surface_to_masks(SDL_Surface *surface,int width,int height,int num_frames,GLuint mask_color,unsigned int stride);
    GLuint *surface_to_textures(SDL_Surface *surface,int width,int height,int num_frames,unsigned int stride);
    unsigned long long int bb_oval_filled(long long int x,long long int y,long long int width,long long int height);
unsigned long long int bb_oval_hollow(long long int x,long long int y,long long int width,long long int height);
unsigned long long int bb_rect_filled(long long int x,long long int y,long long int width,long long int height);
unsigned long long int bb_rect_hollow(long long int x,long long int y,long long int width,long long int height);

}

char LIBKOSHKA_GRAPHICS_ERROR[256];
long long int BB_GRAPHICS_WIDTH;
long long int BB_GRAPHICS_HEIGHT;
long long int BB_ORIGIN_X;
long long int BB_ORIGIN_Y;
const unsigned long long int BB_BACK_BUFFER = 0;
unsigned long long int BB_CURRENT_BUFFER;
double BB_SCALE_X;
double BB_SCALE_Y;
double BB_ORIENTATION;
double BB_ALPHA;
double BB_PI = 3.14159265358979323;
unsigned long long int BB_AUTOMIDHANDLE;
long long int BB_PRIMITIVE_HANDLE_X;
long long int BB_PRIMITIVE_HANDLE_Y;
SDL_Window *WINDOW;
SDL_GLContext GL_CONTEXT;
unsigned long long int BB_COLOR_RED;
unsigned long long int BB_COLOR_GREEN;
unsigned long long int BB_COLOR_BLUE;
unsigned long long int BB_COLOR_ALPHA;
unsigned long long int BB_CLS_COLOR_RED;
unsigned long long int BB_CLS_COLOR_GREEN;
unsigned long long int BB_CLS_COLOR_BLUE;
unsigned long long int BB_CLS_COLOR_ALPHA;
    
GLuint PRIMITIVE_PROGRAM;
GLuint IMAGE_PROGRAM;

GList *GRAPHICS_CACHE;

#define GRAPHICS_TYPE_LINE 1
#define GRAPHICS_TYPE_RECT 2
#define GRAPHICS_TYPE_RECT_HOLLOW 3
#define GRAPHICS_TYPE_OVAL 4
#define GRAPHICS_TYPE_OVAL_HOLLOW 5

typedef struct
{
    unsigned long long int id;
    void *data;
} CachedGraphics;

class CachedRect
{
public:
    unsigned long long int id;
    glm::vec4 vertices[6];
    glm::vec4 color;
};

class CachedRectHollow
{
public:
    unsigned long long int id;
    glm::vec4 vertices[8];
    glm::vec4 color;
};

class CachedLine
{
public:
    unsigned long long int id;
    glm::vec4 vertices[2];
    glm::vec4 color;
};

class CachedOval
{
public:
    static unsigned long long int TOTAL_NUM_VERTICES;
    unsigned long long int id;
    glm::vec4 *vertices;
    unsigned long long int num_vertices;
    glm::vec4 color;
};

class CachedOvalHollow
{
public:
    static unsigned long long int TOTAL_NUM_VERTICES;
    unsigned long long int id;
    glm::vec4 *vertices;
    unsigned long long int num_vertices;
    glm::vec4 color;
};

unsigned long long int CachedOval::TOTAL_NUM_VERTICES;
unsigned long long int CachedOvalHollow::TOTAL_NUM_VERTICES;

extern "C"
{


    void show_bin(unsigned long long int in)
    {
	int i;
	//printf("BIN\n");
	for(i = 0; i < 64; i++)
	{
	    printf("%lld",(long long unsigned int)((in >> ((8 * sizeof(unsigned long long int) - 1) - i)) & 1));
	}
        //printf("\n");
	
    }
    
    unsigned long long int bb_graphics(long long int width,long long int height,unsigned long long int depth, unsigned long long int windowed)
    {
	CachedOval::TOTAL_NUM_VERTICES = 0;
	CachedOvalHollow::TOTAL_NUM_VERTICES = 0;
	GLenum glew_error;
	BB_GRAPHICS_WIDTH = width;
	BB_GRAPHICS_HEIGHT = height;

	BB_CLS_COLOR_RED = 0;
	BB_CLS_COLOR_GREEN = 0;
	BB_CLS_COLOR_BLUE = 0;
	BB_CLS_COLOR_ALPHA = 1;
	glClearColor(0,0,0,1);

	BB_COLOR_RED = 255;
	BB_COLOR_GREEN = 255;
	BB_COLOR_BLUE = 255;
	BB_COLOR_ALPHA = 255;

	BB_ORIGIN_X = 0;
	BB_ORIGIN_Y = 0;

	BB_SCALE_X = 1.0f;
	BB_SCALE_Y = 1.0f;
	
	BB_ORIENTATION = 0.0f;
	BB_ALPHA = 1.0f;

	BB_AUTOMIDHANDLE = 0;
	
	BB_PRIMITIVE_HANDLE_X = 0;
	BB_PRIMITIVE_HANDLE_Y = 0;

	BB_CURRENT_BUFFER = BB_BACK_BUFFER;

	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3);
	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 0);

        WINDOW = SDL_CreateWindow( "Idlewild-Lang Runtime Window", 64, 128, BB_GRAPHICS_WIDTH, BB_GRAPHICS_HEIGHT, SDL_WINDOW_OPENGL);
	if(!WINDOW)
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Unable to create window.\n%s\n",(const char*)SDL_GetError());
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
    
	GL_CONTEXT = SDL_GL_CreateContext(WINDOW);
	if(GL_CONTEXT == NULL)
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Unable to create OpenGL context.\n%s\n",(const char*)SDL_GetError());
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
                        
        glew_error = glewInit();
	if(glew_error != GLEW_OK)
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Unable to initialise GLEW.\n%s\n",(const char*)glewGetErrorString(glew_error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
        }
	
	SDL_GL_SetSwapInterval(0);
	
	load_primitive_program();
	load_image_program();

	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	
	glViewport(0,0,width,height);

	GRAPHICS_CACHE = NULL;
	
	return 0;
    }
    
    unsigned long long int bb_setbuffer(unsigned long long int buffer)
    {
	BB_CURRENT_BUFFER = buffer;
    }

    unsigned long long int bb_backbuffer()
    {
	return BB_BACK_BUFFER;
    }    
    
    unsigned long long int bb_origin(long long int x, long long int y)
    {
	BB_ORIGIN_X = x;
	BB_ORIGIN_Y = y;
	return 0;
    }

    unsigned long long int bb_setscale(double x,double y)
    {
	BB_SCALE_X = x;
	BB_SCALE_Y = y;
	return 0;
    }

    double bb_getscalex()
    {
	return BB_SCALE_X;
    }

    double bb_getscaley()
    {
	return BB_SCALE_Y;
    }
    
    unsigned long long int bb_setorientation(double a)
    {
	BB_ORIENTATION = a;
	return 0;
    }

    double bb_getorientation()
    {
	return BB_ORIENTATION;
    }

    unsigned long long int bb_setalpha(double a)
    {
	BB_ALPHA = a;
	return 0;
    }

    double bb_getalpha()
    {
	return BB_ALPHA;
    }
    
    unsigned long long int bb_cls()
    {
	glClear( GL_COLOR_BUFFER_BIT );
    }

    unsigned long long int bb_loadimage(char *file_name)
    {

	SDL_Surface* surface = IMG_Load(file_name);
	Image *image;
	GLuint texture;
 
	if(surface->format->BytesPerPixel != 4)
	{
	    SDL_FreeSurface(surface);
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s' does not have an alpha channel.",file_name);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
        glGenTextures(1,&texture);
	glBindTexture(GL_TEXTURE_2D,texture);
	
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);

	glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,surface->w,surface->h,0,GL_RGBA,GL_UNSIGNED_BYTE,surface->pixels);
	
	image = (Image*)malloc(sizeof(Image));
	image->textures = (GLuint*)malloc(sizeof(GLuint));
	image->textures[0] = texture;
	image->surface = surface;
	image->width = surface->w;
	image->height = surface->h;
	image->width_frames = 1;
	image->height_frames = 1;
	image->mask_color = 0;
	mask_surface(surface,0,0);
	image->masks = surface_to_masks(surface,surface->w,surface->h,1,0,image->width_frames * image->width);

	image->mask_width = image->width / (8 * sizeof(unsigned long long int));
	image->mask_height = image->height;// / (8 * sizeof(unsigned long long int));

	if(image->width % sizeof(unsigned long long int))
	    image->mask_width++;

	if(image->height % sizeof(unsigned long long int))
	    image->mask_height++;
	
	if(BB_AUTOMIDHANDLE)
	{
	    image->handle_x = image->width / 2;
	    image->handle_y = image->height / 2;
	}
	else
	{
	    image->handle_x = 0;
	    image->handle_y = 0;
	}

	return (unsigned long long int)image;
    }

/*
    unsigned long long int bb_imagescollide(unsigned long long int image1_handle,long long int image1_x,long long int image1_y,long long int frame1,unsigned long long int image2_handle,long long int image2_x,long long int image2_y,long long int frame2)
    {
	Image *image1 = (Image*)image1_handle;
	Image *image2 = (Image*)image2_handle;
	int offset_x,offset_y;
	offset_x = image2_x - image1_x;
	offset_y = image2_y - image1_y;

	int overlap_x = 0,overlap_y = 0;
	int overlap_width = 0,overlap_height = 0;
	int sector_x = 0,sector_y = 0;
	int remainder = 0;
	int collision = 0;
	int x1 = 0,y1 = 0,x2 = 0,y2 = 0;
	
	if(offset_x < 0)
	{
	    if(offset_y < 0)
            {
		if(offset_x + image2->width > 0 && offset_y + image2->height > 0)
		{
		    collision = 1;
		    printf("COLLISION: x negative, y negative\n");
		    overlap_x = 0;
		    overlap_y = 0;
		    overlap_width = image2->width + offset_x;
		    overlap_height = image2->height + offset_y;
		}
	    }
	    else
	    {
		if(offset_x + image2->width > 0 && offset_y < image1->height)
		{
		    collision = 1;
		    printf("COLLISION: x negative, y positive\n");
		    overlap_x = 0;
		    overlap_y = offset_y;
		    overlap_width = image2->width + offset_x;
		    if(offset_y + image2->height < image1->height)
		    {
			overlap_height = image2->height;
		    }
		    else
		    {
			overlap_height = image1->height - offset_y;
		    }
		}
	    }
	
	}
	else
	{
	    if(offset_y < 0)
            {
		if(offset_x < image1->width && offset_y + image2->height > 0)
		{
		    collision = 1;
		    printf("COLLISION: x positive, y negative\n");
		    overlap_x = offset_x;
		    overlap_y = 0;
		    if(offset_x + image2->width > image1->width)
		    {
			overlap_width = image1->width - offset_x;
		    }
		    else
		    {
			overlap_width = image2->width;
		    }
		    overlap_height = image2->height + offset_y;
		    
		}

	    }
	    else
	    {
		if(offset_x < image1->width && offset_y < image1->height)
		{
		    collision = 1;
		    printf("COLLISION: x positive, y positive\n");
		    overlap_x = offset_x;
		    overlap_y = offset_y;
		    if(offset_x + image2->width > image1->width)
		    {
			overlap_width = image1->width - offset_x;
		    }
		    else
		    {
			overlap_width = image2->width;
		    }

		    if(offset_y + image2->height > image1->height)
		    {
			overlap_height = image1->height - offset_y;
		    }
		    else
		    {
			overlap_height = image2->height;

		    }
		}
	    
	    }

	}

	if(!collision)
	{
	    printf("NO COLLISION\n");
	    return 0;
	}

	printf("OVERLAP    : %d,%d\n",overlap_x,overlap_y);
	printf("DIMENSIONS : %d,%d\n",overlap_width,overlap_height);

        int sector_x1 = overlap_x / (8 * sizeof(unsigned long long int));
	int sector_y1 = overlap_y;

	int sector_x2;
	int sector_y2;

	if(offset_x < 0)
	{
	    sector_x2 = abs(offset_x) / (8 * sizeof(unsigned long long int));
	}
	else
	{
	    sector_x2 = 0;
	}

	if(offset_y < 0)
	{
	    sector_y2 = abs(offset_y) / (8 * sizeof(unsigned long long int));
	}
	else
	{
	    sector_y2 = 0;
	}

	int num_sectors_x1 = overlap_width / (8 * sizeof(unsigned long long int));
	if(overlap_width % (8 * sizeof(unsigned long long int)))
	    num_sectors_x1++;

	int num_sectors_y1 = overlap_height;

	int num_sectors_x2,num_sectors_y2;
	
	
        num_sectors_x2 = (overlap_x - offset_x) / (8 * sizeof(unsigned long long int)) + 1;
	if((overlap_x - offset_x) % (8 * sizeof(unsigned long long int)))
	    num_sectors_x2++;

	if(offset_y < 0)
	    num_sectors_y2 = overlap_y + 1;
	else
	    num_sectors_y2 = overlap_height;
	    

	printf("sectors 1 = %d,%d\n",num_sectors_x1,num_sectors_y1);
	printf("sectors 2 = %d,%d\n",num_sectors_x2,num_sectors_y2);

	unsigned long long int img,ref;

	for(y1 = sector_y1; y1 < num_sectors_y1; y1++)
	{
	    for(x1 = sector_x1; x1 < num_sectors_x1; x1++)
	    {
		img = image1->masks[frame1][y1 * image1->mask_width + x1];
		show_bin(img);
	    }
	}
	

/*	
	sector_x = overlap_x / (8 * sizeof(unsigned long long int));
	sector_y = overlap_y;
	
	unsigned long long int mask;
	unsigned long long int anded;
	unsigned long long int shifted;
	unsigned long long int mask1,mask2;
	int num_sectors_x = overlap_width / (8 * sizeof(unsigned long long int));
	if(overlap_width % (8 * sizeof(unsigned long long int)))
	    num_sectors_x++;

	remainder = overlap_width % (8 * sizeof(unsigned long long int));
	int x,y;
        for(y = overlap_y; y < overlap_height; y++)
	{
	    for(x = 0; x < num_sectors_x; x++)
	    {
		
	    }
	}	

    }
*/

    unsigned long long int bb_imagescollide(unsigned long long int image1_handle,long long int image1_x,long long int image1_y,unsigned long long int frame1,unsigned long long int image2_handle,long long int image2_x,long long int image2_y,unsigned long long int frame2)
    {
	Image *image1 = (Image*)image1_handle;
	Image *image2 = (Image*)image2_handle;
	long long int offset_x,offset_y;
	offset_x = image2_x - image1_x;
	offset_y = image2_y - image1_y;

	long long int overlap_x1 = 0,overlap_y1 = 0;
	long long int overlap_x2 = 0,overlap_y2 = 0;
	long long int overlap_width = 0,overlap_height = 0;

	unsigned long long int sector_x = 0,sector_y = 0;
	long long int remainder = 0;
	long long int collision = 0;
	long long int x1 = 0,y1 = 0,x2 = 0,y2 = 0;

	//printf("OFFSET = %lld,%lld\n",offset_x,offset_y);
	
	if(offset_x < 0)
	{
	    if(offset_y < 0)
            {
		if(offset_x + image2->width > 0 && offset_y + image2->height > 0)

		{
		    //exit(1);
                    //printf("1)\n");
		    collision = 1;

		    overlap_x1 = 0;//image1->width - abs(offset_x);
		    overlap_y1 = 0;//image1->height - abs(offset_y);
		    
		    overlap_width = std::min(image1->width,((long long int)image1->width) + offset_x);
		    overlap_height = std::min(image1->height,((long long int)image1->height) + offset_y);

		    overlap_x2 = image2->width - overlap_width;
		    overlap_y2 = image2->height - overlap_height;
		    
		    //printf("overlap width/height = %lld,%lld\n",overlap_width,overlap_height);

	        }
	    }
	    else
	    {
		if(offset_x + image2->width > 0 && offset_y < image1->height)
		{
		    //exit(1);
		    //printf("2)\n");

		    collision = 1;
		    //printf("offset = %lld,%lld\n",offset_x,offset_y);
		    

		    overlap_x1 = 0;
		    overlap_y1 = offset_y;
		    overlap_x2 = -offset_x;
		    overlap_y2 = 0;

		    overlap_width = image2->width + offset_x;
		    overlap_height = image1->height - offset_y;
/*
		    printf("overlap wiz = %lld,%lld\n",overlap_x1,overlap_y1);
		    printf("overlap til = %lld,%lld\n",overlap_x2,overlap_y2);
		    printf("width = %lld\n",overlap_width);
		    printf("height = %lld\n",overlap_height);
*/
		    //exit(1);
		    
		}

	    }
	
	}
	else
	{
	    if(offset_y < 0)
            {
		if(offset_y + image2->height > 0 && offset_x < image1->width)
		{
		    //exit(1);
		    //printf("Yeah\n");
		    //printf("offset y = %d,height = %lld\n",offset_y,image2->height);
		    //printf("image2_y = %lld, image1_y = %lld\n",image2_y,image1_y);
		    collision = 1;
		    overlap_x1 = image1->width - offset_x;
		    overlap_y1 = 0;
		    overlap_x2 = 0;
		    overlap_y2 = image2->height - abs(offset_y);

		    overlap_width = image1->width - (long long int)offset_x;
		    overlap_height = std::min(image1->height,image2->height - abs(offset_y));

		}
	    }
	    else
	    {
		if(offset_x < image1->width && offset_y < image1->height)
		{
		    //exit(1);
		    //printf("4)\n");
		    collision = 1;
		    overlap_x1 = offset_x;
		    overlap_y1 = offset_y;
		    overlap_x2 = 0;
		    overlap_y2 = 0;

		    overlap_width = std::min(image2->width,image1->width - offset_x);
		    overlap_height = std::min(image2->height,image1->height - offset_y);		    
		}
	    }

	}
	
	if(!collision)
	{
	    //printf("NO COLLISION\n");
	    return 0;
	}
	
	//printf("num sectors 1 = %d,%d\n",num_sectors_x1,num_sectors_y1);
	//printf("num sectors 2 = %d,%d\n",num_sectors_x2,num_sectors_y2);

	long long int num_sectors_x,num_sectors_y;

	long long int sector_x1,sector_y1;
	long long int sector_x2,sector_y2;

	sector_x1 = overlap_x1 / (8 * sizeof(unsigned long long int));
	sector_y1 = overlap_y1;

	sector_x2 = overlap_x2 / (8 * sizeof(unsigned long long int));
	sector_y2 = overlap_y2;

	//printf("sectors 1 = %lld,%lld\n",sector_x1,sector_y1);
	//printf("sectors 2 = %lld,%lld\n",sector_x2,sector_y2);	

	//printf("overlap 1 = %lld,%lld\n",overlap_x1,overlap_y1);
	//printf("overlap 2 = %lld,%lld\n",overlap_x2,overlap_y2);

	printf(" ");
	
	num_sectors_x = overlap_width / (8 * sizeof(unsigned long long int));
	if(overlap_width % (8 * sizeof(unsigned long long int)))
	    num_sectors_x++;

	num_sectors_y = overlap_height + 1;	

        //printf("num_sectors = %lld,%lld\n",num_sectors_x,num_sectors_y);
	//printf("mask widths = %lld,%lld\n",image1->mask_width,image2->mask_width);

	unsigned long long int x,y;
	unsigned long long int img1,img2,img3,shifted;
	remainder = abs(offset_x) % (8 * sizeof(unsigned long long int));

	for(y = 0; y < num_sectors_y; y++)
	{
	    for(x = 0; x < num_sectors_x; x++)
	    {
		if((y + sector_y2) * image2->mask_width + x + sector_x2 >= 1024)
		    goto skip;
		
		img1 = image1->masks[frame1][(y + sector_y1) * image1->mask_width + x + sector_x1];
		img2 = image2->masks[frame2][(y + sector_y2) * image2->mask_width + x + sector_x2];

		//printf("\n\n");
		//show_bin(img1);
		//printf("\n");
		//show_bin(img2);
		//printf("\n");

		if(offset_x < 0)
		{
		    shifted = img2 << remainder;
		    //show_bin(shifted);
		    //printf("\n");
		    //exit(1);
		    if(img1 & shifted)
			return 1;//exit(1);
		}
		else
		{
		    shifted = img2 >> remainder;
		    //show_bin(shifted);
		    //exit(1);
		    if(img1 & shifted)
			return 1;
		    //printf("\n");
		}

	    }
	}

      

    skip:
	//exit(1);
	return 0;
	
    }
    

	/*
    unsigned long long int bb_loadanimimage(char *file_name,unsigned long long int width,unsigned long long int height,unsigned long long int first,unsigned long long int num_frames)
    {
	SDL_Surface* surface = IMG_Load(file_name);
	Image *image;
	GLuint *textures;
	int x,y,i,j,k,l;
	unsigned int *pixels;
	unsigned int stride;

	if(surface->w % width || surface->h % height)
	{
	    SDL_FreeSurface(surface);
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s': width/height does not divide exactly into frame count provided.",file_name);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	if(surface->format->BytesPerPixel != 4)
	{
	    SDL_FreeSurface(surface);
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s' does not have an alpha channel.",file_name);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}

	image = (Image*)malloc(sizeof(Image));
	image->textures = (GLuint*)malloc(num_frames * sizeof(GLuint));
	image->surface = surface;
	image->width = width;
	image->height = height;
	image->width_frames = surface->w / width;
	image->height_frames = surface->h / height;
        //printf("surface dims = %d, %d\n",surface->w,surface->h);
	stride = image->width_frames * width;

	k = 0;
	l = 0;

	for(i = 0; i < num_frames; i++)
	{
	    j = 0;
	    pixels = (unsigned int*)malloc(width * height * sizeof(unsigned int));
	    for(y = k; y < k + height; y++)
	    {
		for(x = l; x < l + width; x++)
		{
		    pixels[j] = ((unsigned int*)surface->pixels)[y * stride + x];
		    j++;
                }
	    }

	    if(x == surface->w)
	    {
		l = 0;
		k = y;
	    }
	    else
	    {
	        k = y - height;
		l = x;
	    }

	    glGenTextures(1,image->textures + i);
	    glBindTexture(GL_TEXTURE_2D,image->textures[i]);
	
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
	    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,width,height,0,GL_RGBA,GL_UNSIGNED_BYTE,pixels);

	    free(pixels);
	}
	
	if(BB_AUTOMIDHANDLE)
	{
	    image->handle_x = image->width / 2;
	    image->handle_y = image->height / 2;
	}
	else
	{
	    image->handle_x = 0;
	    image->handle_y = 0;
	}

	return (unsigned long long int)image;

    }
    
    unsigned long long int bb_maskimage(unsigned long long int image_handle,unsigned long long int red,unsigned long long int green,unsigned long long int blue)
    {
	
	GLuint *textures;
	Image *image = (Image*)image_handle;
	int mask = red << 16 | green << 8 | blue;
	int i;
	int num_frames = image->width_frames * image->height_frames;

	for(i = 0; i < num_frames; i++)
	{
	    glDeleteTextures(1,image->textures + i);
	}
	
	for(i = 0; i < (image->surface->w * image->surface->h); i++)
	{
	    if((((int*)image->surface->pixels)[i] & 0x00FFFFFF) == mask)
	    {
		((int*)image->surface->pixels)[i] &= 0x00FFFFFF;
	    }
	}

	for(i = 0; i < num_frames; i++)
	{
	    glGenTextures(1,textures + i);
	    glBindTexture(GL_TEXTURE_2D,texture);

	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);

	    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,image->surface->w,image->surface->h,0,GL_RGBA,GL_UNSIGNED_BYTE,image->surface->pixels);
	    image->texture = texture;
	}
	
    }
    
    unsigned long long int bb_handleimage(unsigned long long int image_handle,long long int x,long long int y)
    {
	Image *image = (Image*)image_handle;
	image->handle_x = x;
	image->handle_y = y;
	return 0;
    }
*/


    unsigned long long int bb_loadanimimage(char *file_name,unsigned long long int width,unsigned long long int height,unsigned long long int first,unsigned long long int num_frames)
    {
	SDL_Surface* surface = IMG_Load(file_name);
	Image *image;
	unsigned int stride;
	//GLuint *textures;
	//int x,y,i,j,k,l;

	if(surface->w % width || surface->h % height)
	{
	    SDL_FreeSurface(surface);
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s': width/height does not divide exactly into frame count provided.",file_name);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	if(surface->format->BytesPerPixel != 4)
	{
	    SDL_FreeSurface(surface);
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s' does not have an alpha channel.",file_name);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}

	mask_surface(surface,0,0);
	
	image = (Image*)malloc(sizeof(Image));
	image->surface = surface;
	image->width = width;
	image->height = height;
	image->width_frames = surface->w / width;
	image->height_frames = surface->h / height;
	image->mask_color = 0;
        stride = image->width_frames * width;
	image->textures = surface_to_textures(surface,width,height,num_frames,stride);//(GLuint*)malloc(num_frames * sizeof(GLuint));
	image->masks = surface_to_masks(surface,width,height,num_frames,0,stride);
	image->mask_width = image->width / (8 * sizeof(unsigned long long int));
	image->mask_height = image->height;// / (8 * sizeof(unsigned long long int));

	if(BB_AUTOMIDHANDLE)
	{
	    image->handle_x = image->width / 2;
	    image->handle_y = image->height / 2;
	}
	else
	{
	    image->handle_x = 0;
	    image->handle_y = 0;
	}

	return (unsigned long long int)image;

    }

    unsigned long long int bb_maskimage(unsigned long long int image_handle,unsigned long long int red,unsigned long long int green,unsigned long long int blue)
    {
	Image *image = (Image*)image_handle;
	GLuint mask_color = red << 16 | green << 8 | blue;
	GLuint prev_mask_color = image->mask_color;
	mask_surface(image->surface,mask_color,prev_mask_color);
	image->masks = surface_to_masks(image->surface,image->width,image->height,image->width_frames * image->height_frames,mask_color,image->width_frames * image->width);
	image->textures = surface_to_textures(image->surface,image->width,image->height,image->width_frames * image->height_frames,image->width_frames * image->width);
    }

    GLuint *surface_to_textures(SDL_Surface *surface,int width,int height,int num_frames,unsigned int stride)
    {
	int i = 0;
	int j = 0;
	int k = 0;
	int l = 0;
	int x = 0;
	int y = 0;
	unsigned int *pixels;
	//unsigned int stride;
	GLuint *textures = (GLuint*)malloc(num_frames * sizeof(GLuint));;
	
	for(i = 0; i < num_frames; i++)
	{
	    j = 0;
	    pixels = (unsigned int*)malloc(width * height * sizeof(unsigned int));
	    for(y = k; y < k + height; y++)
	    {
		for(x = l; x < l + width; x++)
		{
		    pixels[j] = ((unsigned int*)surface->pixels)[y * stride + x];
		    j++;
                }
	    }

	    if(x == surface->w)
	    {
		l = 0;
		k = y;
	    }
	    else
	    {
	        k = y - height;
		l = x;
	    }

	    glGenTextures(1,textures + i);
	    glBindTexture(GL_TEXTURE_2D,textures[i]);
	
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
	    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,width,height,0,GL_RGBA,GL_UNSIGNED_BYTE,pixels);

	    free(pixels);
	}
	
	return textures;

    }
    
    unsigned long long int **surface_to_masks(SDL_Surface *surface,int width,int height,int num_frames,GLuint mask_color,unsigned int stride)
    {
	int i = 0;
	//int j = 0;
	int k = 0;
	int l = 0;
	int x = 0;
	int y = 0;
	unsigned long long int *pixels;
	unsigned int mask_width,mask_height;
	unsigned int offset,sector_x,sector_y;

	mask_width = width / (8 * sizeof(unsigned long long int));// + 1;
	mask_height = height;// / sizeof(unsigned long long int);

	if(width % (8 * sizeof(unsigned long long int)))
	    mask_width++;

	if(height % (8 * sizeof(unsigned long long int)))
	    mask_height++;
	
	unsigned long long int **masks = (unsigned long long int**)malloc(num_frames * sizeof(unsigned long long int));
	memset(masks,0,num_frames * sizeof(unsigned long long int));
        
	for(i = 0; i < num_frames; i++)
	{
	    //j = 0;
	    pixels = (unsigned long long int*)malloc((mask_width + 0) * mask_height * sizeof(unsigned long long int));
	    //printf("kk %u %u\n",mask_width,mask_height);
	    memset(pixels,0,(mask_width + 0) * mask_height * sizeof(unsigned long long int));
	    //exit(1);
	    for(y = k; y < k + height; y++)
	    {
		offset = 0;
		//printf("Start x loop\n\n");
		for(x = l; x < l + width; x++)
		{
		    //printf("write %d\n",x);
		    //offset = x % (8 * sizeof(unsigned long long int));
                    
		    sector_x = x / (8 * sizeof(unsigned long long int));
		    //printf("sector_x = %d\n",sector_x);
		    sector_y = y - k;/// (8 * sizeof(unsigned long long int));
		    //printf("sector_y = %d\n",sector_y);
		    if((((int*)surface->pixels)[y * stride + x] & 0x00FFFFFF) == mask_color)
		    {
		        //printf("0");
			//printf("%lld",(pixels[sector_y * mask_width + sector_x] >> offset) & 1);
		    }
		    else
		    {
			//printf("1");
			pixels[sector_y * (mask_width - 1) + sector_x] |= (0x8000000000000000 >> offset);

		        //printf("%lld",(long long unsigned int)((pixels[sector_y * mask_width + sector_x] >> ((8 * sizeof(unsigned long long int) - 1) - offset)) & 1));
		    }

		    offset = (offset + 1) % (8 * sizeof(unsigned long long int));
		    //j++;
                }
	        //printf("\n");
	    }

	    if(x == surface->w)
	    {
		l = 0;
		k = y;
	    }
	    else
	    {
	        k = y - height;
		l = x;
	    }

	    masks[i] = pixels;
	    //free(pixels);
	}
	//printf("\n\n");
	int m,s,z;
	z = 0;
	for(m = 0; m < num_frames; m++)
	{
	    for(s = 0; s < (mask_width -1) * mask_height; s++)
	    {
		z++;
		//show_bin(masks[m][s]);
		if(z == 4)
		{
		    z = 0;
		    //printf("\n");
		}

	    }
	    //printf("\n");

	}
	return masks;


    }


    
    unsigned long long int mask_surface(SDL_Surface *surface,GLuint mask_color,GLuint prev_mask_color)
    {

	int i;
	
	for(i = 0; i < (surface->w * surface->h); i++)
	{
	    if((((int*)surface->pixels)[i] & 0x00FFFFFF) == mask_color)
	    {
		((int*)surface->pixels)[i] &= 0x00FFFFFF;
	    }
	    else if((((int*)surface->pixels)[i] & 0x00FFFFFF) == prev_mask_color)
	    {
	    	((int*)surface->pixels)[i] |= 0xFF000000;
	    }
	}
    }
    
    unsigned long long int bb_handleimage(unsigned long long int image_handle,long long int x,long long int y)
    {
	Image *image = (Image*)image_handle;
	image->handle_x = x;
	image->handle_y = y;
	return 0;
    }
    
    unsigned long long int bb_imagexhandle(unsigned long long int image_handle)
    {
	Image *image = (Image*)image_handle;
	return image->handle_x;
    }

    unsigned long long int bb_imageyhandle(unsigned long long int image_handle)
    {
	Image *image = (Image*)image_handle;
	return image->handle_y;
    }

    unsigned long long int bb_imagewidth(unsigned long long int image_handle)
    {
	Image *image = (Image*)image_handle;
	return image->width;
    }

    unsigned long long int bb_imageheight(unsigned long long int image_handle)
    {
	Image *image = (Image*)image_handle;
	return image->height;
    }
    
    unsigned long long int bb_midhandle(unsigned long long int image_handle)
    {
	Image *image = (Image*)image_handle;
	image->handle_x = image->width / 2;
	image->handle_y = image->height / 2;
	return 0;
    }

    unsigned long long int bb_automidhandle(unsigned long long int value)
    {
	BB_AUTOMIDHANDLE = value;
	return 0;
    }

char *dummy_error(GLuint error)
{
    return (char*)"";
}

    /*
    unsigned long long int bb_drawimage(unsigned long long int image_handle,long long int x,long long int y,unsigned long long int frame)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLfloat x1,y1,x2,y2,a;
	GLint x1_handle;
	GLint y1_handle;
	GLint x2_handle;
	GLint y2_handle;
	GLint a_handle;
	GLint texture_handle;
	GLint position_handle;
	Image *image = (Image*)image_handle;

	GLfloat vertices [] = {-1.0f,-1.0f,0.0f,1.0f,
			       1.0f,-1.0f,0.0f,1.0f,
			       -1.0f,1.0f,0.0f,1.0f,
			       1.0f,1.0f,0.0f,1.0f};
	
	GLushort indices [] = {0,1,2,3};
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;

	glm::mat4 scale_matrix1(
	    0.5 * float(image->width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(image->height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);	

	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	    
	float offset_by_handle_x = ((0.5f * (image->width * BB_SCALE_X) - image->handle_x) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (image->height * BB_SCALE_Y) - image->handle_y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;

	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	glm::mat4 translation_matrix1(
	    1.0f,0.0f,0.0f,0.5 * image->width - image->handle_x,
	    0.0f,1.0f,0.0f,-0.5f * image->height + image->handle_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 translation_matrix2(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * translation_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix2;
        	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,sizeof(vertices),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,sizeof(indices),indices,GL_STATIC_DRAW);
	
	glUseProgram(IMAGE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.1 %d %s\n",error,dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(IMAGE_PROGRAM, "uMVPMatrix");
	texture_handle = glGetUniformLocation(IMAGE_PROGRAM, "texture_");
	x1_handle = glGetUniformLocation(IMAGE_PROGRAM,"x1");
	y1_handle = glGetUniformLocation(IMAGE_PROGRAM,"y1");
	x2_handle = glGetUniformLocation(IMAGE_PROGRAM,"x2");
	y2_handle = glGetUniformLocation(IMAGE_PROGRAM,"y2");
        a_handle = glGetUniformLocation(IMAGE_PROGRAM,"a");
	position_handle = glGetAttribLocation(IMAGE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	x1 = 0.0f;
	y1 = 0.0f;
	x2 = 1.0f;
	y2 = 1.0f;
	a = BB_ALPHA;
	
	glUniform1f(x1_handle,x1);
	glUniform1f(y1_handle,y1);
	glUniform1f(x2_handle,x2);
	glUniform1f(y2_handle,y2);
	glUniform1f(a_handle,a);
	
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,image->textures[frame]);
	glUniform1i(texture_handle,0);
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
    }
    */


    unsigned long long int bb_drawimage(unsigned long long int image_handle,long long int x,long long int y,unsigned long long int frame)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLfloat x1,y1,x2,y2,a;
	GLint x1_handle;
	GLint y1_handle;
	GLint x2_handle;
	GLint y2_handle;
	GLint a_handle;
	GLint texture_handle;
	GLint position_handle;
	Image *image = (Image*)image_handle;

	GLfloat vertices [] = {-1.0f,-1.0f,0.0f,1.0f,
			       1.0f,-1.0f,0.0f,1.0f,
			       -1.0f,1.0f,0.0f,1.0f,
			       1.0f,1.0f,0.0f,1.0f};
	
	GLushort indices [] = {0,1,2,3};

	/*
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;

	glm::mat4 scale_matrix1(
	    0.5 * float(image->width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(image->height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);	

	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	    
	float offset_by_handle_x = ((0.5f * (image->width * BB_SCALE_X) - image->handle_x) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (image->height * BB_SCALE_Y) - image->handle_y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;

	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	glm::mat4 translation_matrix1(
	    1.0f,0.0f,0.0f,0.5 * image->width - image->handle_x,
	    0.0f,1.0f,0.0f,-0.5f * image->height + image->handle_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 translation_matrix2(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * translation_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix2;
	*/
//###
	
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;
	
	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	float offset_by_handle_x = ((0.5f * (image->width * BB_SCALE_X) - (image->handle_x * BB_SCALE_X)) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (image->height * BB_SCALE_Y) - (image->handle_y * BB_SCALE_Y)) / (BB_GRAPHICS_HEIGHT)) * 2.0f;
	
	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	
	glm::mat4 translation_matrix(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_handle_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_handle_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 scale_matrix1(
	    0.5 * float(image->width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(image->height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;

//###

	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,sizeof(vertices),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,sizeof(indices),indices,GL_STATIC_DRAW);
	
	glUseProgram(IMAGE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.1 %d %s\n",error,dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(IMAGE_PROGRAM, "uMVPMatrix");
	texture_handle = glGetUniformLocation(IMAGE_PROGRAM, "texture_");
	x1_handle = glGetUniformLocation(IMAGE_PROGRAM,"x1");
	y1_handle = glGetUniformLocation(IMAGE_PROGRAM,"y1");
	x2_handle = glGetUniformLocation(IMAGE_PROGRAM,"x2");
	y2_handle = glGetUniformLocation(IMAGE_PROGRAM,"y2");
        a_handle = glGetUniformLocation(IMAGE_PROGRAM,"a");
	position_handle = glGetAttribLocation(IMAGE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	x1 = 0.0f;
	y1 = 0.0f;
	x2 = 1.0f;
	y2 = 1.0f;
	a = BB_ALPHA;
	
	glUniform1f(x1_handle,x1);
	glUniform1f(y1_handle,y1);
	glUniform1f(x2_handle,x2);
	glUniform1f(y2_handle,y2);
	glUniform1f(a_handle,a);
	
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,image->textures[frame]);
	glUniform1i(texture_handle,0);
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
    }
    
    unsigned long long int bb_drawimagerect(unsigned long long int image_handle,long long int x,long long int y,long long int rect_x1,long long int rect_y1,long long int rect_width,long long int rect_height,long long int frame)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLfloat x1,y1,x2,y2;
	GLint x1_handle;
	GLint y1_handle;
	GLint x2_handle;
	GLint y2_handle;
	GLint texture_handle;
	GLint position_handle;
	Image *image = (Image*)image_handle;

	GLfloat vertices [] = {-1.0f,-1.0f,0.0f,1.0f,
			       1.0f,-1.0f,0.0f,1.0f,
			       -1.0f,1.0f,0.0f,1.0f,
			       1.0f,1.0f,0.0f,1.0f};
	
	GLushort indices [] = {0,1,2,3};
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;

	glm::mat4 scale_matrix1(
	    0.5 * float(image->width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(image->height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);	

	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	    
	float offset_by_handle_x = ((0.5f * (image->width * BB_SCALE_X) - image->handle_x) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (image->height * BB_SCALE_Y) - image->handle_y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;

	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	glm::mat4 translation_matrix1(
	    1.0f,0.0f,0.0f,0.5 * image->width - image->handle_x,
	    0.0f,1.0f,0.0f,-0.5f * image->height + image->handle_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 translation_matrix2(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * translation_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix2;
	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,sizeof(vertices),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,sizeof(indices),indices,GL_STATIC_DRAW);
	
	glUseProgram(IMAGE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(IMAGE_PROGRAM, "uMVPMatrix");
	texture_handle = glGetUniformLocation(IMAGE_PROGRAM, "texture_");
	x1_handle = glGetUniformLocation(IMAGE_PROGRAM,"x1");
	y1_handle = glGetUniformLocation(IMAGE_PROGRAM,"y1");
	x2_handle = glGetUniformLocation(IMAGE_PROGRAM,"x2");
	y2_handle = glGetUniformLocation(IMAGE_PROGRAM,"y2");
	position_handle = glGetAttribLocation(IMAGE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	x1 = float(rect_x1) / image->width;
	y1 = float(rect_y1) / image->height;
	x2 = float(rect_x1 + rect_width) / image->width;
	y2 = float(rect_y1 + rect_height) / image->height;
	
	glUniform1f(x1_handle,x1);
	glUniform1f(y1_handle,y1);
	glUniform1f(x2_handle,x2);
	glUniform1f(y2_handle,y2);
	
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,image->textures[frame]);
	glUniform1i(texture_handle,0);
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_TRIANGLE_STRIP,4,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
    }
    
    unsigned long long int bb_oval(long long int x,long long int y,long long int width,long long int height,unsigned long long int filled)
    {
	if(!filled)
	    bb_oval_hollow(x,y,width,height);
	else
	    bb_oval_filled(x,y,width,height);
    }

    unsigned long long int bb_line(long long int x1,long long int y1,long long int x2,long long int y2)
    {
	CachedGraphics *cached_graphics;
	CachedLine *cached_line;
      	GLuint vertex_buffer;
	GLuint index_buffer;
	GLenum error;
	GLuint position_handle;
	GLuint color_handle;
	GLuint vertex_stride;
	GLfloat x1_adjusted,y1_adjusted,x2_adjusted,y2_adjusted;
	GLuint indices [] = {0,1};
	GLuint matrix_handle;
	GLuint color_buffer;
	GLfloat *colors;

	x1_adjusted = (x1 + BB_ORIGIN_X);
	y1_adjusted = (BB_GRAPHICS_HEIGHT - y1 - BB_ORIGIN_Y);
	x2_adjusted = (x2 + BB_ORIGIN_X) - x1_adjusted;
	y2_adjusted = (BB_GRAPHICS_HEIGHT - y2 - BB_ORIGIN_Y) - y1_adjusted;

        GLfloat *vertex_data;
	glm::vec4 vertices[2];
	
        glm::mat4 matrix;

	glm::mat4 translationMatrix(
	    1.0f,0.0f,0.0f,-(0.5 * BB_GRAPHICS_WIDTH),
	    0.0f,1.0f,0.0f,-(0.5 * BB_GRAPHICS_HEIGHT),
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scaleMatrix(
		 2.0f / BB_GRAPHICS_WIDTH,0.0f,0.0f,0.0f,
		 0.0f,2.0f / BB_GRAPHICS_HEIGHT,0.0f,0.0f,
		 0.0f,0.0f,1.0f,0.0f,
		 0.0f,0.0f,0.0f,1.0f);

	matrix = translationMatrix * scaleMatrix;

	cached_line = new CachedLine;
	cached_graphics = (CachedGraphics*)malloc(sizeof(CachedGraphics));
	cached_graphics->data = (void*)cached_line;
	cached_graphics->id = GRAPHICS_TYPE_LINE;
	cached_graphics->data = cached_line;
	
        cached_line->vertices[0] = glm::vec4(x1_adjusted,y1_adjusted,0.0f,1.0f) * matrix;
	cached_line->vertices[1] = glm::vec4(x1_adjusted + x2_adjusted,y1_adjusted + y2_adjusted,0.0f,1.0f) * matrix;
	cached_line->color = glm::vec4(BB_COLOR_RED / 255.0,BB_COLOR_GREEN / 255.0,BB_COLOR_BLUE / 255.0,BB_ALPHA);

	GRAPHICS_CACHE = g_list_prepend(GRAPHICS_CACHE,(void*)cached_graphics);

    }

    GList *batch_line(GList* list)
    {
	unsigned long long int num_items = 0;
	GLfloat *vertex_data;
	CachedGraphics *cached_graphics;
	CachedLine *cached_line = (CachedLine*)list->data;
	int i,j;
	GList *start = list;
	GList *finish;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLuint *indices;
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLuint color_buffer;
       	
	while(list && ((CachedGraphics*)list->data)->id == GRAPHICS_TYPE_LINE)
	{
	    num_items++;
	    list = g_list_next(list);
	}
        finish = list;
	
	int size = 2 * 4 * sizeof(GLfloat);
	indices = (GLuint*)malloc(2 * num_items * sizeof(GLuint));

	for(i = 0; i < (2 * num_items); i++)
	{
	    indices[i] = i;
	}
	
        vertex_data = (GLfloat*)malloc(2 * size * num_items);
	list = start;

	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_line = (CachedLine*)cached_graphics->data;
	    for(j = 0; j < 2; j++)
	    {
		memcpy(vertex_data + (i * 2 + j) * sizeof(GLfloat),glm::value_ptr(cached_line->vertices[j]),4 * sizeof(GLfloat));
	    }

	    list = g_list_next(list);   
	}

	list = start;
        GLfloat *colors = vertex_data + ((size * num_items) / 4);
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_line = (CachedLine*)cached_graphics->data;
	    for(j = 0; j < 2; j++)
	    {
		memcpy(colors + (i * 2 + j) * sizeof(GLfloat),glm::value_ptr(cached_line->color),4 * sizeof(GLfloat));
	    }
	    list = g_list_next(list);   
	}
	

	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * 2 * num_items,vertex_data,GL_STATIC_DRAW);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,2 * sizeof(GLuint) * num_items,indices,GL_STATIC_DRAW);

	glGenBuffers(1,&color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * 2*  num_items,colors,GL_STATIC_DRAW);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	color_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "color");
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,4 * sizeof(GLfloat),(void*)0);
        glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glVertexAttribPointer(color_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(color_handle);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	

	glDrawElements(GL_LINES,2 * num_items,GL_UNSIGNED_INT,(void*)0);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.2 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	glDisableVertexAttribArray(position_handle);
	glDisableVertexAttribArray(color_handle);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&color_buffer);
	glDeleteBuffers(1,&index_buffer);
	return finish;
    }
    
    
    unsigned long long int bb_oval_filled(long long int x,long long int y,long long int width,long long int height)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	CachedGraphics *cached_graphics;
	CachedOval *cached_oval;
	int i;
	
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;
	
	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	float offset_by_handle_x = ((0.5f * (width * BB_SCALE_X) - BB_PRIMITIVE_HANDLE_X) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (height * BB_SCALE_Y) - BB_PRIMITIVE_HANDLE_Y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;
	
	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	
	glm::mat4 translation_matrix(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_handle_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_handle_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	float major_axis = width > height ? width : height;
	float minor_axis = width > height ? height : width;
	long long unsigned int perimeter = 0.09 * 2.0f * BB_PI * sqrt((major_axis * major_axis + minor_axis * minor_axis) / 2.0);
	if(perimeter < 3) perimeter = 3;
	if(perimeter > 26) perimeter = 26;
	float interval = (2.0f * BB_PI) / perimeter;
	
	glm::mat4 scale_matrix1(
	    0.5 * float(width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;

	cached_oval = new CachedOval;
        cached_oval->vertices = new glm::vec4[perimeter * 3 + 3];

	cached_graphics = (CachedGraphics*)malloc(sizeof(CachedGraphics));
	cached_graphics->data = (void*)cached_oval;
	cached_graphics->id = GRAPHICS_TYPE_OVAL;
	int j;

	for(i = 0; i < perimeter; i++)
	{
	    j = i * 3;
	    cached_oval->vertices[j] = glm::vec4(cos(interval * i),sin(interval * i),0.0f,1.0f) * matrix;
	    cached_oval->vertices[j + 1] = glm::vec4(cos(interval * (i + 1)),sin(interval * (i + 1)),0.0f,1.0f) * matrix;
	    cached_oval->vertices[j + 2] = glm::vec4(0.0f,0.0f,0.0f,1.0f) * matrix;
	}

	cached_oval->num_vertices = perimeter * 3 + 3;
	CachedOval::TOTAL_NUM_VERTICES += perimeter * 3 + 3;
        cached_oval->color = glm::vec4(BB_COLOR_RED / 255.0,BB_COLOR_GREEN / 255.0,BB_COLOR_BLUE / 255.0,BB_ALPHA);
	GRAPHICS_CACHE = g_list_prepend(GRAPHICS_CACHE,(void*)cached_graphics);
    }

    GList *batch_oval_filled(GList *list)
    {
	unsigned long long int num_items = 0;
	GLfloat *vertex_data;
	GLfloat *color_data;
	GLfloat *p;
	CachedGraphics *cached_graphics;
	CachedOval *cached_oval = (CachedOval*)list->data;
	int i,j;
	GList *start = list;
	GList *finish;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLuint *indices;
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLuint color_buffer;
       	
	while(list && ((CachedGraphics*)list->data)->id == GRAPHICS_TYPE_OVAL)
	{
	    num_items++;
	    list = g_list_next(list);
	}
	finish = list;

	indices = (GLuint*)malloc(CachedOval::TOTAL_NUM_VERTICES * sizeof(GLuint));
        
	for(i = 0; i < CachedOval::TOTAL_NUM_VERTICES; i++)
	{
	    indices[i] = i;
	}
	
        vertex_data = (GLfloat*)malloc(CachedOval::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat));
	list = start;
	p = vertex_data;
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_oval = (CachedOval*)cached_graphics->data;
	    for(j = 0; j < cached_oval->num_vertices; j++)
	    {
		memcpy(p + j * 4,glm::value_ptr(cached_oval->vertices[j]),4 * sizeof(GLfloat));
	    }

	    p += cached_oval->num_vertices * 4;
	    list = g_list_next(list);   
	}
       
	list = start;
        color_data = (GLfloat*)malloc(CachedOval::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat));
        p = color_data;
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_oval = (CachedOval*)cached_graphics->data;
	    for(j = 0; j < cached_oval->num_vertices; j++)
	    {
		memcpy(p + j * 4,glm::value_ptr(cached_oval->color),4 * sizeof(GLfloat));
	    }
	    p += cached_oval->num_vertices * 4;
	    list = g_list_next(list);   
	}

        glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,CachedOval::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat),vertex_data,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,CachedOval::TOTAL_NUM_VERTICES * sizeof(GLuint),indices,GL_STATIC_DRAW);

	glGenBuffers(1,&color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glBufferData(GL_ARRAY_BUFFER,CachedOval::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat),color_data,GL_STATIC_DRAW);
        
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.6 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	color_handle =  glGetAttribLocation(PRIMITIVE_PROGRAM, "color");
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glVertexAttribPointer(color_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(color_handle);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_TRIANGLES,CachedOval::TOTAL_NUM_VERTICES,GL_UNSIGNED_INT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertex_data);
	free(color_data);
	free(indices);

	glDisableVertexAttribArray(position_handle);
	glDisableVertexAttribArray(color_handle);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&color_buffer);
	glDeleteBuffers(1,&index_buffer);

	return finish;
    }


    unsigned long long int bb_oval_hollow(long long int x,long long int y,long long int width,long long int height)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	CachedGraphics *cached_graphics;
	CachedOvalHollow *cached_oval;
	int i;
	
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;
	
	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	float offset_by_handle_x = ((0.5f * (width * BB_SCALE_X) - BB_PRIMITIVE_HANDLE_X) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (height * BB_SCALE_Y) - BB_PRIMITIVE_HANDLE_Y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;
	
	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	
	glm::mat4 translation_matrix(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_handle_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_handle_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	float major_axis = width > height ? width : height;
	float minor_axis = width > height ? height : width;
	long long int perimeter = 0.09 * 2.0f * BB_PI * sqrt((major_axis * major_axis + minor_axis * minor_axis) / 2.0);
	if(perimeter < 3) perimeter = 3;
	if(perimeter > 26) perimeter = 26;
	float interval = (2.0f * BB_PI) / perimeter;
	
	glm::mat4 scale_matrix1(
	    0.5 * float(width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;

	cached_oval = new CachedOvalHollow;
        cached_oval->vertices = new glm::vec4[perimeter * 2 + 2];

	cached_graphics = (CachedGraphics*)malloc(sizeof(CachedGraphics));
	cached_graphics->data = (void*)cached_oval;
	cached_graphics->id = GRAPHICS_TYPE_OVAL_HOLLOW;
	int j;

	for(i = 0; i < perimeter; i++)
	{
	    j = i * 2;
	    cached_oval->vertices[j] = glm::vec4(cos(interval * i),sin(interval * i),0.0f,1.0f) * matrix;
	    cached_oval->vertices[j + 1] = glm::vec4(cos(interval * (i + 1)),sin(interval * (i + 1)),0.0f,1.0f) * matrix;
	}

	cached_oval->num_vertices = perimeter * 2 + 2;
	CachedOvalHollow::TOTAL_NUM_VERTICES += perimeter * 2 + 2;

        cached_oval->color = glm::vec4(BB_COLOR_RED / 255.0,BB_COLOR_GREEN / 255.0,BB_COLOR_BLUE / 255.0,BB_ALPHA);
	GRAPHICS_CACHE = g_list_prepend(GRAPHICS_CACHE,(void*)cached_graphics);

    }

    GList *batch_oval_hollow(GList *list)
    {
	unsigned long long int num_items = 0;
	GLfloat *vertex_data;
	GLfloat *color_data;
	GLfloat *p;
	CachedGraphics *cached_graphics;
	CachedOvalHollow *cached_oval = (CachedOvalHollow*)list->data;
	int i,j;
	GList *start = list;
	GList *finish;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLuint *indices;
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLuint color_buffer;
       	
	while(list && ((CachedGraphics*)list->data)->id == GRAPHICS_TYPE_OVAL_HOLLOW)
	{
	    num_items++;
	    list = g_list_next(list);
	}
	finish = list;

	indices = (GLuint*)malloc(CachedOvalHollow::TOTAL_NUM_VERTICES * sizeof(GLuint));
        
	for(i = 0; i < CachedOvalHollow::TOTAL_NUM_VERTICES; i++)
	{
	    indices[i] = i;
	}
	
        vertex_data = (GLfloat*)malloc(CachedOvalHollow::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat));
	list = start;
	p = vertex_data;
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_oval = (CachedOvalHollow*)cached_graphics->data;
	    for(j = 0; j < cached_oval->num_vertices; j++)
	    {
		memcpy(p + j * 4,glm::value_ptr(cached_oval->vertices[j]),4 * sizeof(GLfloat));
	    }

	    p += cached_oval->num_vertices * 4;
	    list = g_list_next(list);   
	}
       
	list = start;
        color_data = (GLfloat*)malloc(CachedOvalHollow::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat));
        p = color_data;
	
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_oval = (CachedOvalHollow*)cached_graphics->data;
	    for(j = 0; j < cached_oval->num_vertices; j++)
	    {
		memcpy(p + j * 4,glm::value_ptr(cached_oval->color),4 * sizeof(GLfloat));
	    }

	    p += cached_oval->num_vertices * 4;
	    list = g_list_next(list);   
	}

        glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,CachedOvalHollow::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat),vertex_data,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,CachedOvalHollow::TOTAL_NUM_VERTICES * sizeof(GLuint),indices,GL_STATIC_DRAW);

	glGenBuffers(1,&color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glBufferData(GL_ARRAY_BUFFER,CachedOvalHollow::TOTAL_NUM_VERTICES * 4 * sizeof(GLfloat),color_data,GL_STATIC_DRAW);
        
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.6 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	color_handle =  glGetAttribLocation(PRIMITIVE_PROGRAM, "color");
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glVertexAttribPointer(color_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(color_handle);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_LINES,CachedOvalHollow::TOTAL_NUM_VERTICES,GL_UNSIGNED_INT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertex_data);
	free(color_data);
	free(indices);

	glDisableVertexAttribArray(position_handle);
	glDisableVertexAttribArray(color_handle);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&color_buffer);
	glDeleteBuffers(1,&index_buffer);

	return finish;
    }
    


    unsigned long long int bb_rect(long long int x,long long int y,long long int width,long long int height,unsigned long long int filled)
    {
	if(!filled)
	    bb_rect_hollow(x,y,width,height);
	else
	    bb_rect_filled(x,y,width,height);
    }    

    unsigned long long int bb_rect_filled(long long int x,long long int y,long long int width,long long int height)
    {
	CachedGraphics *cached_graphics;
	CachedRect *cached_rect;
	int i;
	
	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;
	
	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	float offset_by_handle_x = ((0.5f * (width * BB_SCALE_X) - BB_PRIMITIVE_HANDLE_X) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (height * BB_SCALE_Y) - BB_PRIMITIVE_HANDLE_Y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;
	
	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	
	glm::mat4 translation_matrix(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_handle_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_handle_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 scale_matrix1(
	    0.5 * float(width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;
	
	cached_rect = new CachedRect;
	cached_graphics = (CachedGraphics*)malloc(sizeof(CachedGraphics));
	cached_graphics->data = (void*)cached_rect;
	cached_graphics->id = GRAPHICS_TYPE_RECT;
	cached_graphics->data = cached_rect;
	
        cached_rect->vertices[0] = glm::vec4(-1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[1] = glm::vec4(-1.0,1.0,0.0,1.0) * matrix;
        cached_rect->vertices[2] = glm::vec4(1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[3] = glm::vec4(-1.0,1.0,0.0,1.0) * matrix;
        cached_rect->vertices[4] = glm::vec4(1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[5] = glm::vec4(1.0,1.0,0.0,1.0) * matrix;
        cached_rect->color = glm::vec4(BB_COLOR_RED / 255.0,BB_COLOR_GREEN / 255.0,BB_COLOR_BLUE / 255.0,BB_ALPHA);

	//printf("vertices x = %f,%f,%f,%f,%f,%f\n",(cached_rect->vertices[0].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH,(cached_rect->vertices[1].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH,(cached_rect->vertices[2].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH,(cached_rect->vertices[3].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH,(cached_rect->vertices[4].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH,(cached_rect->vertices[5].x + 1.0) / 2.0 * BB_GRAPHICS_WIDTH);
	
	GRAPHICS_CACHE = g_list_prepend(GRAPHICS_CACHE,(void*)cached_graphics);
	
    }

    GList *batch_rect_filled(GList *list)
    {
	unsigned long long int num_items = 0;
	GLfloat *vertex_data;
	GLfloat *color_data;
	CachedGraphics *cached_graphics;
	CachedRect *cached_rect = (CachedRect*)list->data;
	int i,j;
	GList *start = list;
	GList *finish;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLuint *indices;
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLuint color_buffer;
       	
	while(list && ((CachedGraphics*)list->data)->id == GRAPHICS_TYPE_RECT)
	{
	    num_items++;
	    list = g_list_next(list);
	}
        finish = list;
	
	int size = 6 * 4 * sizeof(GLfloat);
	indices = (GLuint*)malloc(6 * num_items * sizeof(GLuint));

	for(i = 0; i < (6 * num_items); i++)
	{
	    indices[i] = i;
	}
	
        vertex_data = (GLfloat*)malloc(2 * size * num_items);

	list = start;

	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_rect = (CachedRect*)cached_graphics->data;
	    for(j = 0; j < 6; j++)
	    {
		memcpy(vertex_data + (i * 6 + j) * sizeof(GLfloat),glm::value_ptr(cached_rect->vertices[j]),4 * sizeof(GLfloat));
	    }

	    list = g_list_next(list);   
	}

	list = start;
        color_data = vertex_data + ((size * num_items) / 4);
	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_rect = (CachedRect*)cached_graphics->data;
	    for(j = 0; j < 6; j++)
	    {
		memcpy(color_data + (i * 6 + j) * sizeof(GLfloat),glm::value_ptr(cached_rect->color),4 * sizeof(GLfloat));
	    }
	    list = g_list_next(list);   
	}
	
        glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * num_items,vertex_data,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,6 * num_items * sizeof(GLuint),indices,GL_STATIC_DRAW);

	glGenBuffers(1,&color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * num_items,color_data,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.6 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	color_handle =  glGetAttribLocation(PRIMITIVE_PROGRAM, "color");
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glVertexAttribPointer(color_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(color_handle);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_TRIANGLES,6 * num_items,GL_UNSIGNED_INT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertex_data);
	free(indices);

	glDisableVertexAttribArray(position_handle);
	glDisableVertexAttribArray(color_handle);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&color_buffer);
	glDeleteBuffers(1,&index_buffer);

	return finish;
    }
    



    unsigned long long int bb_rect_hollow(long long int x,long long int y,long long int width,long long int height)
    {
	CachedGraphics *cached_graphics;
	CachedRectHollow *cached_rect;
	int i;

	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotation_matrix = glm::rotate(((float)BB_ORIENTATION) / 360.0f * 2.0f * ((float)BB_PI),axis);
        glm::mat4 matrix;
	
	float offset_by_x = (((float)x) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_y = (((float)y) / BB_GRAPHICS_HEIGHT) * 2.0f;

	float offset_by_handle_x = ((0.5f * (width * BB_SCALE_X) - BB_PRIMITIVE_HANDLE_X) / (BB_GRAPHICS_WIDTH)) * 2.0f;
	float offset_by_handle_y = ((0.5f * (height * BB_SCALE_Y) - BB_PRIMITIVE_HANDLE_Y) / (BB_GRAPHICS_HEIGHT)) * 2.0f;
	
	float offset_by_origin_x = (((float)BB_ORIGIN_X) / BB_GRAPHICS_WIDTH) * 2.0f;
	float offset_by_origin_y = (((float)BB_ORIGIN_Y) / BB_GRAPHICS_HEIGHT) * 2.0f;
	
	glm::mat4 translation_matrix(
	    1.0f,0.0f,0.0f,-1.0f + offset_by_x + offset_by_handle_x + offset_by_origin_x,
	    0.0f,1.0f,0.0f,1.0f - offset_by_y - offset_by_handle_y - offset_by_origin_y,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);
	
	glm::mat4 scale_matrix1(
	    0.5 * float(width) * BB_SCALE_X,0.0f,0.0f,0.0f,
	    0.0f,0.5 * float(height) * BB_SCALE_Y,0.0f,0.0f,
	    0.0f,0.0f,1.0f,0.0f,
	    0.0f,0.0f,0.0f,1.0f);

	glm::mat4 scale_matrix2(
	    (2.0f / BB_GRAPHICS_WIDTH),0.0f,0.0f,0.0f,
	     0.0f,(2.0f / BB_GRAPHICS_HEIGHT),0.0f,0.0f,
	     0.0f,0.0f,1.0f,0.0f,
	     0.0f,0.0f,0.0f,1.0f);
	
        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;

	cached_rect = new CachedRectHollow;
	cached_graphics = (CachedGraphics*)malloc(sizeof(CachedGraphics));
	cached_graphics->data = (void*)cached_rect;
	cached_graphics->id = GRAPHICS_TYPE_RECT_HOLLOW;
	cached_graphics->data = cached_rect;

        cached_rect->vertices[0] = glm::vec4(-1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[1] = glm::vec4(-1.0,1.0,0.0,1.0) * matrix;
        cached_rect->vertices[2] = glm::vec4(-1.0,1.0,0.0,1.0) * matrix;
        cached_rect->vertices[3] = glm::vec4(1.0,1.0,0.0,1.0) * matrix;

        cached_rect->vertices[4] = glm::vec4(1.0,1.0,0.0,1.0) * matrix;
        cached_rect->vertices[5] = glm::vec4(1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[6] = glm::vec4(1.0,-1.0,0.0,1.0) * matrix;
        cached_rect->vertices[7] = glm::vec4(-1.0,-1.0,0.0,1.0) * matrix;

        cached_rect->color = glm::vec4(BB_COLOR_RED / 255.0,BB_COLOR_GREEN / 255.0,BB_COLOR_BLUE / 255.0,BB_ALPHA);
	GRAPHICS_CACHE = g_list_prepend(GRAPHICS_CACHE,(void*)cached_graphics);
	
    }

    GList *batch_rect_hollow(GList *list)
    {
	unsigned long long int num_items = 0;
	GLfloat *vertex_data;
	GLfloat *color_data;
	CachedGraphics *cached_graphics;
	CachedRectHollow *cached_rect = (CachedRectHollow*)list->data;
	int i,j;
	GList *start = list;
	GList *finish;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLuint *indices;
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLuint color_buffer;
       	
	while(list && ((CachedGraphics*)list->data)->id == GRAPHICS_TYPE_RECT_HOLLOW)
	{
	    num_items++;
	    list = g_list_next(list);
	}
        finish = list;
	
	int size = 8 * 4 * sizeof(GLfloat);
	indices = (GLuint*)malloc(8 * num_items * sizeof(GLuint));

	for(i = 0; i < (8 * num_items); i++)
	{
	    indices[i] = i;
	}
	
        vertex_data = (GLfloat*)malloc(2 * size * num_items);

	list = start;

	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_rect = (CachedRectHollow*)cached_graphics->data;
 
	    for(j = 0; j < 8; j++)
	    {
		memcpy(vertex_data + (i * 8 + j) * sizeof(GLfloat),glm::value_ptr(cached_rect->vertices[j]),4 * sizeof(GLfloat));
	    }

	    list = g_list_next(list);   
	}

	list = start;

	list = start;
        color_data = vertex_data + ((size * num_items) / 4);

	for(i = 0; i < num_items; i++)
	{
	    cached_graphics = (CachedGraphics*)list->data;
	    cached_rect = (CachedRectHollow*)cached_graphics->data;
	    for(j = 0; j < 8; j++)
	    {
		memcpy(color_data + (i * 8 + j) * sizeof(GLfloat),glm::value_ptr(cached_rect->color),4 * sizeof(GLfloat));
	    }
	    list = g_list_next(list);   
	}
	
        glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * num_items,vertex_data,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,8 * num_items * sizeof(GLuint),indices,GL_STATIC_DRAW);

	glGenBuffers(1,&color_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glBufferData(GL_ARRAY_BUFFER,size * num_items,color_data,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx.6 %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	color_handle =  glGetAttribLocation(PRIMITIVE_PROGRAM, "color");
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,color_buffer);
	glVertexAttribPointer(color_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(color_handle);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_LINES,8 * num_items,GL_UNSIGNED_INT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertex_data);
	free(indices);

	glDisableVertexAttribArray(position_handle);
	glDisableVertexAttribArray(color_handle);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&color_buffer);
	glDeleteBuffers(1,&index_buffer);

	return finish;
    }
    
    void load_primitive_program()
    {
	GLuint linked;
	GLsizei log_length;
        GLchar log[256];
	GLenum error;
	int vertex_shader_size;
	int fragment_shader_size;
	
	const GLchar *vertex_shader_code [] =
	     {"#version 130\n",
	      "in vec4 vPosition;",
	      "in vec4 color;",
	      "out vec4 fragment_color;",
	      "void main() {",
	      "  gl_Position = vPosition;",
	      "  fragment_color = color;",
	      "}",NULL};

	const GLchar *fragment_shader_code [] =
	     {"#version 130\n",
	      "in vec4 fragment_color;",
	      "void main() {",
	      "  gl_FragColor = fragment_color;",
	      "}",NULL};

        vertex_shader_size = 0;
	while(vertex_shader_code[vertex_shader_size]) vertex_shader_size++;
        fragment_shader_size = 0;
	while(fragment_shader_code[fragment_shader_size]) fragment_shader_size++;
	
        GLuint vertex_shader = load_shader(GL_VERTEX_SHADER,vertex_shader_code,vertex_shader_size);
	GLuint fragment_shader = load_shader(GL_FRAGMENT_SHADER,fragment_shader_code,fragment_shader_size);

	PRIMITIVE_PROGRAM = glCreateProgram();
	glAttachShader(PRIMITIVE_PROGRAM, vertex_shader);
	glAttachShader(PRIMITIVE_PROGRAM, fragment_shader);
	glLinkProgram(PRIMITIVE_PROGRAM);
	glGetProgramiv(PRIMITIVE_PROGRAM, GL_LINK_STATUS, (GLint *)&linked);

	glGetProgramInfoLog(PRIMITIVE_PROGRAM,255,&log_length,log);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Error loading primitive shaders. %s\n%s\n",dummy_error(error),log);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}

    }

    void load_image_program()
    {
	GLuint linked;
	GLsizei log_length;
        GLchar log[256];
	GLenum error;
	int vertex_shader_size;
	int fragment_shader_size;
	
	const GLchar *vertex_shader_code [] =
	     {"#version 130\n",
	      "attribute vec4 vPosition;",
	      "uniform mat4 uMVPMatrix;",
	      "varying vec2 textureCoordinates;",
	      "void main() {",
	      "  gl_Position = vPosition * uMVPMatrix;",
	      "  textureCoordinates = vec2(-vPosition) * vec2(0.5) + vec2(0.5);",
	      "}",NULL};

	const GLchar *fragment_shader_code [] =
	     {"#version 130\n",
	      "uniform sampler2D texture_;",
	      "uniform float x1;",
	      "uniform float y1;",
	      "uniform float x2;",
	      "uniform float y2;",
	      "uniform float a;",
	      "varying vec2 textureCoordinates;",
	      "void main() {",
              "  if(1.0 - textureCoordinates.x >= x1 && 1.0 - textureCoordinates.x <= x2 && textureCoordinates.y >= y1 && textureCoordinates.y <= y2)",
              "  {",
	      "    vec4 k = texture(texture_,vec2(1.0 - textureCoordinates.x,textureCoordinates.y));",
	      "    if(k.a != 0.0)",
              "      k.a = a;",
	      "    gl_FragColor = k;",
	      "  }",
	      "  else",
	      "  {",
	      "    gl_FragColor = vec4(0.0f,0.0f,0.0f,0.0f);",	      
	      "  }",
	      "}",NULL};
	
        vertex_shader_size = 0;
	while(vertex_shader_code[vertex_shader_size]) vertex_shader_size++;
        fragment_shader_size = 0;
	while(fragment_shader_code[fragment_shader_size]) fragment_shader_size++;
	
        GLuint vertex_shader = load_shader(GL_VERTEX_SHADER,vertex_shader_code,vertex_shader_size);

	char infoLog[256];
	GLsizei length_;
	glGetShaderInfoLog(vertex_shader,
 	256,
 	&length_,
 	infoLog);

	GLuint fragment_shader = load_shader(GL_FRAGMENT_SHADER,fragment_shader_code,fragment_shader_size);

	IMAGE_PROGRAM = glCreateProgram();
	glAttachShader(IMAGE_PROGRAM, vertex_shader);
	glAttachShader(IMAGE_PROGRAM, fragment_shader);
	glLinkProgram(IMAGE_PROGRAM);
	glGetProgramiv(IMAGE_PROGRAM, GL_LINK_STATUS, (GLint *)&linked);

	glGetProgramInfoLog(IMAGE_PROGRAM,255,&log_length,log);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Error loading image shaders. %s\n%s\n",dummy_error(error),log);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
    }
    
    GLuint load_shader(GLenum type, const GLchar **shaderCode,int num_lines)
    {

        GLuint shader;
	GLenum error;
	GLint compiled;
	GLsizei log_length;
        GLchar log[256];
	
	shader = glCreateShader(type);

	glShaderSource(shader, num_lines, shaderCode, NULL);
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR, "Unable to assign shader source.  %s\n", dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
       
	glCompileShader(shader);
	glGetShaderiv(shader,GL_COMPILE_STATUS,&compiled);
	glGetShaderInfoLog(shader,256,&log_length,log);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Unable to compile shader. %s\n",log);
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}

	return shader;
}

    void free_cached_rect(void *it)
    {
	delete ((CachedRect*)it);
    }

    void free_cached_oval(void *it)
    {
	delete [] ((CachedOval*)it)->vertices;
	delete ((CachedOval*)it);
    }

    void free_cached_graphics(void *address)
    {
	CachedGraphics *graphics = (CachedGraphics*)address;
	switch(graphics->id)
	{
	case GRAPHICS_TYPE_LINE:
	{
	    delete ((CachedLine*)graphics->data);
	    break;
	}
	case GRAPHICS_TYPE_RECT:
	{
	    delete ((CachedRect*)graphics->data);
	    break;
	}
	case GRAPHICS_TYPE_RECT_HOLLOW:
	{
	    delete ((CachedRectHollow*)graphics->data);
	    break;
	}
	case GRAPHICS_TYPE_OVAL:
	{
	    delete [] ((CachedOval*)graphics->data)->vertices;
	    delete ((CachedOval*)graphics->data);
	    break;
	}
	case GRAPHICS_TYPE_OVAL_HOLLOW:
	{
	    delete [] ((CachedOvalHollow*)graphics->data)->vertices;
	    delete ((CachedOvalHollow*)graphics->data);
	    break;
	}       
	}
	free((CachedGraphics*)graphics);
    }

    unsigned long long int bb_flip(unsigned long long int sync)
    {
	GList *list = GRAPHICS_CACHE;
	CachedGraphics *cached_graphics;
	GRAPHICS_CACHE = g_list_reverse(GRAPHICS_CACHE);
	list = GRAPHICS_CACHE;
	if(list)
	{
	    while(list)
	    {
		cached_graphics = (CachedGraphics*)list->data;

		switch(cached_graphics->id)
		{
		case GRAPHICS_TYPE_LINE:
		{
		    list = batch_line(list);
		    break;
		}
		case GRAPHICS_TYPE_RECT:
		{
		    list = batch_rect_filled(list);
		    break;
		}
		case GRAPHICS_TYPE_OVAL:
		{
		    list = batch_oval_filled(list);
		    break;
		}
		case GRAPHICS_TYPE_RECT_HOLLOW:
		{
		    list = batch_rect_hollow(list);
		    break;
		}

		case GRAPHICS_TYPE_OVAL_HOLLOW:
		{
		    list = batch_oval_hollow(list);
		    break;
		}
		
		default:
		{
		    list = g_list_next(list);
		    break;
		}

		}
		
	    }
	    
	}
	g_list_free_full(GRAPHICS_CACHE,free_cached_graphics);
	GRAPHICS_CACHE = NULL;
        CachedOval::TOTAL_NUM_VERTICES = 0;
	CachedOvalHollow::TOTAL_NUM_VERTICES = 0;
	SDL_GL_SwapWindow(WINDOW);
    }    
    
    unsigned long long int bb_clscolor(unsigned long long int red,unsigned long long int green,unsigned long long int blue, unsigned long long int alpha)
    {
	BB_CLS_COLOR_RED = red;
	BB_CLS_COLOR_GREEN = green;
	BB_CLS_COLOR_BLUE = blue;
	BB_CLS_COLOR_ALPHA = alpha;
	glClearColor(BB_CLS_COLOR_RED / 255.0f,BB_CLS_COLOR_GREEN / 255.0f,BB_CLS_COLOR_BLUE / 255.0f,BB_CLS_COLOR_ALPHA / 255.0f);
	return 0;
    }    
    
    unsigned long long int bb_color(unsigned long long int red,unsigned long long int green,unsigned long long int blue, unsigned long long int alpha)
    {
	BB_COLOR_RED = red;
	BB_COLOR_GREEN = green;
	BB_COLOR_BLUE = blue;
	BB_COLOR_ALPHA = alpha;
	return 0;
    }    
    
}
