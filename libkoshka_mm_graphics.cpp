/*

Copyright (c) 2014-2015, Clockwork Dev Studio
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

extern "C"
{
    void load_primitive_program();
    void load_image_program();
    GLuint load_shader(GLenum type, const GLchar **shaderCode,int num_lines);
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

extern "C"
{
    unsigned long long int bb_graphics(long long int width,long long int height,unsigned long long int depth, unsigned long long int windowed)
    {
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
	
	SDL_GL_SetSwapInterval(1);
	
	load_primitive_program();
	load_image_program();

	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);
	glViewport(0,0,width,height);
	
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
	image->texture = texture;
	image->surface = surface;
	image->width = surface->w;
	image->height = surface->h;

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
	GLuint texture;
	Image *image = (Image*)image_handle;
	int mask = red << 16 | green << 8 | blue;
	glDeleteTextures(1,&(image->texture));

	for(int i = 0; i < (image->surface->w * image->surface->h); i++)
	{
	    if((((int*)image->surface->pixels)[i] & 0x00FFFFFF) == mask)
	    {
		((int*)image->surface->pixels)[i] &= 0x00FFFFFF;
	    }
	}
	
        glGenTextures(1,&texture);
	glBindTexture(GL_TEXTURE_2D,texture);

	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);

	glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,image->surface->w,image->surface->h,0,GL_RGBA,GL_UNSIGNED_BYTE,image->surface->pixels);
        image->texture = texture;
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

    unsigned long long int bb_drawimage(unsigned long long int image_handle,long long int x,long long int y)
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
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
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

	x1 = 0.0f;
	y1 = 0.0f;
	x2 = 1.0f;
	y2 = 1.0f;
	
	glUniform1f(x1_handle,x1);
	glUniform1f(y1_handle,y1);
	glUniform1f(x2_handle,x2);
	glUniform1f(y2_handle,y2);
	
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D,image->texture);
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
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
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
	glBindTexture(GL_TEXTURE_2D,image->texture);
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
    
    unsigned long long int bb_oval_filled(long long int x,long long int y,long long int width,long long int height)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLushort *indices;
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
	long long int perimeter = 2.0f * BB_PI * sqrt((major_axis * major_axis + minor_axis * minor_axis) / 2.0);
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
	
	vertices = (GLfloat*)malloc(4 * (perimeter) * sizeof(GLfloat));
	indices = (GLushort*)malloc((perimeter) * sizeof(GLushort));

        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;
	
	vertices[0] = 0.0;
	vertices[1] = 0.0;
	vertices[2] = 0.0;
	vertices[3] = 1.0;
	indices[0] = 0;
	
	for(i = 0; i < perimeter; i++)
	{
	    vertices[i * 4] = cos(interval * i);
	    vertices[i * 4 + 1] = sin(interval * i);
	    vertices[i * 4 + 2] = 0.0f;
	    vertices[i * 4 + 3] = 1.0f;
	    indices[i] = i;
	}

	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,4 * (perimeter) * sizeof(GLfloat),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,(perimeter) * sizeof(GLushort),indices,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "uMVPMatrix");
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	color_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "vColor");
        GLfloat color[4] = {BB_COLOR_RED / 255.0f,BB_COLOR_GREEN / 255.0f,BB_COLOR_BLUE / 255.0f,BB_COLOR_ALPHA / 255.0f};
	glUniform4fv(color_handle, 1, color);
	
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);	
	
	glDrawElements(GL_TRIANGLE_FAN,perimeter,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertices);
	free(indices);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
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
	GLushort *indices;
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
	long long int perimeter = 2.0f * BB_PI * sqrt((major_axis * major_axis + minor_axis * minor_axis) / 2.0);
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
	
	vertices = (GLfloat*)malloc(4 * perimeter *  sizeof(GLfloat));
	indices = (GLushort*)malloc(perimeter * sizeof(GLushort));

        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;

	for(i = 0; i < perimeter; i++)
	{
	    vertices[i * 4] = cos(interval * i);
	    vertices[i * 4 + 1] = sin(interval * i);
	    vertices[i * 4 + 2] = 0.0f;
	    vertices[i * 4 + 3] = 1.0f;
	    indices[i] = i;
	}
	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,4 * perimeter * sizeof(GLfloat),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,perimeter * sizeof(GLushort),indices,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "uMVPMatrix");
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	color_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "vColor");
        GLfloat color[4] = {BB_COLOR_RED / 255.0f,BB_COLOR_GREEN / 255.0f,BB_COLOR_BLUE / 255.0f,BB_COLOR_ALPHA / 255.0f};
	glUniform4fv(color_handle, 1, color);
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_LINE_LOOP,perimeter,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertices);
	free(indices);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
    }

    unsigned long long int bb_rect(long long int x,long long int y,long long int width,long long int height,unsigned long long int filled)
    {
	if(!filled)
	    bb_rect_hollow(x,y,width,height);
	else
	    bb_rect_filled(x,y,width,height);
    }
    
    unsigned long long int bb_rect_hollow(long long int x,long long int y,long long int width,long long int height)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLushort *indices;
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
	
	vertices = (GLfloat*)malloc(4 * 4 * sizeof(GLfloat));
	indices = (GLushort*)malloc(4 * sizeof(GLushort));

        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;
    
    vertices[0 * 4 + 0] = -1.0f;
    vertices[0 * 4 + 1] = -1.0f;
    vertices[0 * 4 + 2] = 0.0f;
    vertices[0 * 4 + 3] = 1.0f;
    indices[0] = 0;
    
    vertices[1 * 4 + 0] = -1.0f;
    vertices[1 * 4 + 1] = 1.0f;
    vertices[1 * 4 + 2] = 0.0f;
    vertices[1 * 4 + 3] = 1.0f;    
    indices[1] = 1;

    vertices[2 * 4 + 0] = 1.0f;
    vertices[2 * 4 + 1] = 1.0f;
    vertices[2 * 4 + 2] = 0.0f;
    vertices[2 * 4 + 3] = 1.0f; 
    indices[2] = 2;

    vertices[3 * 4 + 0] = 1.0f;
    vertices[3 * 4 + 1] = -1.0f;
    vertices[3 * 4 + 2] = 0.0f;
    vertices[3 * 4 + 3] = 1.0f;
    indices[3] = 3;
	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,4 * 4 * sizeof(GLfloat),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,4 * sizeof(GLushort),indices,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "uMVPMatrix");
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	color_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "vColor");
        GLfloat color[4] = {BB_COLOR_RED / 255.0f,BB_COLOR_GREEN / 255.0f,BB_COLOR_BLUE / 255.0f,BB_COLOR_ALPHA / 255.0f};
	glUniform4fv(color_handle, 1, color);
	
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,4,GL_FLOAT,0,sizeof(GLfloat) * 4,(void*)0);
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	
	glDrawElements(GL_LINE_LOOP,4,GL_UNSIGNED_SHORT,(void*)0);
	
	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"%s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	free(vertices);
	free(indices);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
    }
    
    unsigned long long int bb_rect_filled(long long int x,long long int y,long long int width,long long int height)
    {
	GLuint error;
	GLuint vertex_buffer;
	GLuint index_buffer;
	GLint matrix_handle;
	GLint position_handle;
	GLuint color_handle;
	GLfloat *vertices;
	GLushort *indices;
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
	
	vertices = (GLfloat*)malloc(4 * 4 * sizeof(GLfloat));
	indices = (GLushort*)malloc(4 * sizeof(GLushort));

        matrix = scale_matrix1 * rotation_matrix * scale_matrix2 * translation_matrix;
    
    vertices[0 * 4 + 0] = -1.0f;
    vertices[0 * 4 + 1] = -1.0f;
    vertices[0 * 4 + 2] = 0.0f;
    vertices[0 * 4 + 3] = 1.0f;
    indices[0] = 0;
    
    vertices[1 * 4 + 0] = -1.0f;
    vertices[1 * 4 + 1] = 1.0f;
    vertices[1 * 4 + 2] = 0.0f;
    vertices[1 * 4 + 3] = 1.0f;    
    indices[1] = 1;

    vertices[2 * 4 + 0] = 1.0f;
    vertices[2 * 4 + 1] = -1.0f;
    vertices[2 * 4 + 2] = 0.0f;
    vertices[2 * 4 + 3] = 1.0f; 
    indices[2] = 2;

    vertices[3 * 4 + 0] = 1.0f;
    vertices[3 * 4 + 1] = 1.0f;
    vertices[3 * 4 + 2] = 0.0f;
    vertices[3 * 4 + 3] = 1.0f;
    indices[3] = 3;
	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,4 * 4 * sizeof(GLfloat),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,4 * sizeof(GLushort),indices,GL_STATIC_DRAW);
	
	glUseProgram(PRIMITIVE_PROGRAM);

	error = glGetError();
	if( error != GL_NO_ERROR )
	{
	    sprintf(LIBKOSHKA_GRAPHICS_ERROR,"xxx. %s\n",dummy_error(error));
	    bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
	}
	
	matrix_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "uMVPMatrix");
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));

	color_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "vColor");
        GLfloat color[4] = {BB_COLOR_RED / 255.0f,BB_COLOR_GREEN / 255.0f,BB_COLOR_BLUE / 255.0f,BB_COLOR_ALPHA / 255.0f};
	glUniform4fv(color_handle, 1, color);
	
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
	free(vertices);
	free(indices);
	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
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
	      "attribute vec4 vPosition;",
	      "uniform mat4 uMVPMatrix;",
	      "void main() {",
	      "  gl_Position = vPosition * uMVPMatrix;",
	      "}",NULL};

	const GLchar *fragment_shader_code [] =
	     {"#version 130\n",
	      "uniform vec4 vColor;",
	      "void main() {",
	      "  gl_FragColor = vColor;",
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
	      "varying vec2 textureCoordinates;",
	      "void main() {",
              "  if(1.0 - textureCoordinates.x >= x1 && 1.0 - textureCoordinates.x <= x2 && textureCoordinates.y >= y1 && textureCoordinates.y <= y2)",
              "  {",
	      "    gl_FragColor = texture(texture_,vec2(1.0 - textureCoordinates.x,textureCoordinates.y));",
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
    
    unsigned long long int bb_flip(unsigned long long int sync)
    {
	SDL_GL_SwapWindow(WINDOW);
    }    

    unsigned long long int bb_line(long long int x1,long long int y1,long long int x2,long long int y2)
    {
      	GLuint vertex_buffer;
	GLuint index_buffer;
	GLenum error;
	GLuint position_handle;
	GLuint color_handle;
	GLuint vertex_stride;
	GLfloat x1_adjusted,y1_adjusted,x2_adjusted,y2_adjusted;
	GLuint indices [] = {0,1};
	GLuint matrix_handle;

    x1_adjusted = (x1 + BB_ORIGIN_X);
	y1_adjusted = (BB_GRAPHICS_HEIGHT - y1 - BB_ORIGIN_Y);
	x2_adjusted = (x2 + BB_ORIGIN_X) - x1_adjusted;
	y2_adjusted = (BB_GRAPHICS_HEIGHT - y2 - BB_ORIGIN_Y) - y1_adjusted;
    
	GLfloat vertices [] = {x1_adjusted,y1_adjusted,0.0f,x1_adjusted + x2_adjusted,y1_adjusted + y2_adjusted,0.0f};

	glm::vec3 axis(0.0f,0.0f,-1.0f);
	glm::mat4 rotationMatrix = glm::rotate(0.0f,axis);
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

	matrix = translationMatrix * rotationMatrix * scaleMatrix;
	
	glGenBuffers(1,&vertex_buffer);
	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glBufferData(GL_ARRAY_BUFFER,sizeof(vertices),vertices,GL_STATIC_DRAW);

	glGenBuffers(1,&index_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER,sizeof(indices),indices,GL_STATIC_DRAW);

	vertex_stride = 3 * sizeof(GLfloat);

	glUseProgram(PRIMITIVE_PROGRAM);

	matrix_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "uMVPMatrix");
	position_handle = glGetAttribLocation(PRIMITIVE_PROGRAM, "vPosition");
	
	glUniformMatrix4fv(matrix_handle,1,0, glm::value_ptr(matrix));
	
	glEnableVertexAttribArray(position_handle);

	glBindBuffer(GL_ARRAY_BUFFER,vertex_buffer);
	glVertexAttribPointer(position_handle,3,GL_FLOAT,0,vertex_stride,(void*)0);
        glEnableVertexAttribArray(position_handle);
	
	color_handle = glGetUniformLocation(PRIMITIVE_PROGRAM, "vColor");

        GLfloat color[4] = {BB_COLOR_RED / 255.0f,BB_COLOR_GREEN / 255.0f,BB_COLOR_BLUE / 255.0f,BB_COLOR_ALPHA / 255.0f};
	
	glUniform4fv(color_handle, 1, color);

	
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,index_buffer);	
	glDrawArrays(GL_LINES, 0, 2);

	glDeleteBuffers(1,&vertex_buffer);
	glDeleteBuffers(1,&index_buffer);
	glDisableVertexAttribArray(position_handle);
	return 0;
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
