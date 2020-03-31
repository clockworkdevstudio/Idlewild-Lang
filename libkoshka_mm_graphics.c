/*

Copyright (c) 2014-2017, Clockwork Dev Studio
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

extern double DPI_SCALE;

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL2_gfxPrimitives.h>
#if MAC_OS==1

#include "libkoshka_mm_io.h"
#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>

#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "libkoshka_mm.h"
#if MAC_OS==1
#include "it_quacks_like_glib.h"
#else
#include <glib.h>
#endif

#ifndef MIN
#define MIN(A,B) ((A) < (B)) ? (A) : (B)
#endif
#ifndef MAX
#define MAX(A,B) ((A) > (B)) ? (A) : (B)
#endif

extern "C"
{
void bb_init_libkoshka_core(unsigned long long int max_gosub_depth);
void bb_final_libkoshka_core();
void load_primitive_program();
void load_image_program();
unsigned long long int bb_freeimage(unsigned long long int image_handle);
unsigned long long int mask_surface(SDL_Surface *surface,unsigned long long int mask_color,unsigned long long int prev_mask_color);
unsigned long long int **surface_to_masks(SDL_Surface *surface,int width,int height,unsigned long long int mask_color);
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
SDL_Renderer *RENDERER;
unsigned long long int BB_COLOR_RED;
unsigned long long int BB_COLOR_GREEN;
unsigned long long int BB_COLOR_BLUE;
unsigned long long int BB_COLOR_ALPHA;
unsigned long long int BB_CLS_COLOR_RED;
unsigned long long int BB_CLS_COLOR_GREEN;
unsigned long long int BB_CLS_COLOR_BLUE;
unsigned long long int BB_CLS_COLOR_ALPHA;
int SURFACE_BYTE_ORDER = 0;
int SBO_RGBA = 1;
int SBO_BGRA = 2;
double ASPECT_RATIO;
long long int VIRTUAL_WIDTH,VIRTUAL_HEIGHT;
double LETTERBOX_WIDTH;

/*
#define GRAPHICS_TYPE_LINE 1
#define GRAPHICS_TYPE_RECT 2
#define GRAPHICS_TYPE_RECT_HOLLOW 3
#define GRAPHICS_TYPE_OVAL 4
#define GRAPHICS_TYPE_OVAL_HOLLOW 5
#define GRAPHICS_TYPE_IMAGE 6
#define TRANSFORMATION_TYPE_SCALE 7
#define TRANSFORMATION_TYPE_ORIENTATION 8
#define TRANSFORMATION_TYPE_ALPHA 9
*/

extern "C"
{
void show_bin(unsigned long long int in)
{
    int i;
    for(i = 0; i < 64; i++)
    {
        printf("%lld",(long long unsigned int)((in >> ((8 * sizeof(unsigned long long int) - 1) - i)) & 1));
    }
}

unsigned long long int bb_countgfxmodes()
{
    return (unsigned long long int)SDL_GetNumDisplayModes(0);
}

unsigned long long int bb_gfxmodewidth(unsigned long long int index)
{
    SDL_DisplayMode display_mode;
    SDL_GetDisplayMode(0,index,&display_mode);
    return (unsigned long long int)display_mode.w;
}

unsigned long long int bb_gfxmodeheight(unsigned long long int index)
{
    SDL_DisplayMode display_mode;
    SDL_GetDisplayMode(0,index,&display_mode);
    return (unsigned long long int)display_mode.h;
}

unsigned long long int bb_gfxmodedepth(unsigned long long int index)
{
    return -1;
}

unsigned long long int bb_graphics(long long int width,long long int height,unsigned long long int depth, long long int windowed)
{
    BB_GRAPHICS_WIDTH = width;
    BB_GRAPHICS_HEIGHT = height;
    
    BB_CLS_COLOR_RED = 0;
    BB_CLS_COLOR_GREEN = 0;
    BB_CLS_COLOR_BLUE = 0;
    BB_CLS_COLOR_ALPHA = 1;
    
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

    if(windowed > 0)
    {
        WINDOW = SDL_CreateWindow( "Idlewild-Lang Runtime Window", 64, 128, width, height,0);
    }
    else
    {
        WINDOW = SDL_CreateWindow( "Idlewild-Lang Runtime Window", 0, 0, width, height,  SDL_WINDOW_FULLSCREEN_DESKTOP);
    }

    if(!WINDOW)
    {
        sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Unable to create window.\n%s\n",(const char*)SDL_GetError());
        bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
    }

    //int actual_width,actual_height;
    //SDL_GL_GetDrawableSize(WINDOW,&actual_width,&actual_height);
    //BB_GRAPHICS_WIDTH = actual_width;
    //BB_GRAPHICS_HEIGHT = actual_height;


    RENDERER = SDL_CreateRenderer(WINDOW, -1, SDL_RENDERER_ACCELERATED);
    SDL_SetRenderDrawBlendMode(RENDERER,SDL_BLENDMODE_BLEND);
    ASPECT_RATIO = double(BB_GRAPHICS_HEIGHT) / height;

    VIRTUAL_WIDTH = width;
    VIRTUAL_HEIGHT = height;
    //LETTERBOX_WIDTH = (BB_GRAPHICS_WIDTH - (VIRTUAL_WIDTH * ASPECT_RATIO)) / 2.0;
    //GRAPHICS_CACHE = NULL;
    return 1;
}

unsigned long long int bb_endgraphics()
{
    SDL_DestroyWindow(WINDOW);
    return 0;
}

unsigned long long int bb_graphicswidth()
{
    return BB_GRAPHICS_WIDTH;

}

unsigned long long int bb_graphicsheight()
{
    return BB_GRAPHICS_HEIGHT;
}

unsigned long long int bb_setbuffer(unsigned long long int buffer)
{
    BB_CURRENT_BUFFER = buffer;
    return  0;
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
    SDL_RenderSetScale(RENDERER,BB_SCALE_X,BB_SCALE_Y);
    return 1;
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
    return 1;
}

double bb_getorientation()
{
    return BB_ORIENTATION;
}

unsigned long long int bb_setalpha(double a)
{
    BB_COLOR_ALPHA = 255 * a;
    return 1;
}

double bb_getalpha()
{
    return ((double)BB_COLOR_ALPHA) / 255.0;
}

unsigned long long int bb_cls()
{
   SDL_SetRenderDrawColor(RENDERER,BB_CLS_COLOR_RED, BB_CLS_COLOR_GREEN,BB_CLS_COLOR_BLUE,BB_CLS_COLOR_ALPHA);
   SDL_RenderClear(RENDERER);
   return 0;
}

unsigned long long int bb_loadimage(char *file_name)
{
    SDL_Surface* surface = IMG_Load(file_name);
    Image *image;

    if(surface->format->BytesPerPixel != 4)
    {
        SDL_FreeSurface(surface);
        sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image '%s' does not have an alpha channel.",file_name);
        bb_fatal_error(LIBKOSHKA_GRAPHICS_ERROR);
    }

    if(surface->format->Rmask == 0xFF0000)
    {
        SURFACE_BYTE_ORDER = SBO_BGRA;

    }
    else if(surface->format->Rmask == 0xFF)
    {
        SURFACE_BYTE_ORDER = SBO_RGBA;
    }
    else
    {
        sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image format not recognised.");
        exit(1);
    }

    image = (Image*)malloc(sizeof(Image));
    image->surface = surface;
    mask_surface(surface,0,0);
    image->texture = SDL_CreateTextureFromSurface(RENDERER,image->surface);
    image->width = surface->w;
    image->height = surface->h;
    image->width_frames = 1;
    image->height_frames = 1;
    image->mask_color = 0;
    image->masks = surface_to_masks(surface,surface->w,surface->h,0);

    image->mask_width = image->width / (8 * sizeof(unsigned long long int));
    image->mask_height = image->height;

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

unsigned long long int bb_loadanimimage(char *file_name,unsigned long long int width,unsigned long long int height,unsigned long long int first,unsigned long long int num_frames)
{
    SDL_Surface* surface = IMG_Load(file_name);
    Image *image;
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

    if(surface->format->Rmask == 0xFF0000)
    {
        SURFACE_BYTE_ORDER = SBO_BGRA;

    }
    else if(surface->format->Rmask == 0xFF)
    {
        SURFACE_BYTE_ORDER = SBO_RGBA;
    }
    else
    {
        sprintf(LIBKOSHKA_GRAPHICS_ERROR,"Image format not recognised.");
        exit(1);
    }

    image = (Image*)malloc(sizeof(Image));
    image->surface = surface;
    mask_surface(surface,0,0);
    image->texture = SDL_CreateTextureFromSurface(RENDERER,image->surface);
    image->width = width;
    image->height = height;
    image->width_frames = surface->w / width;
    image->height_frames = surface->h / height;
    image->mask_color = 0;
    stride = image->width_frames * width;
    image->masks = surface_to_masks(surface,width,height,0);
    image->mask_width = image->width / (8 * sizeof(unsigned long long int));
    image->mask_height = image->height;

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

unsigned long long int bb_freeimage(unsigned long long int image_handle)
{
    Image *image = (Image*)image_handle;
    int num_frames = image->width_frames * image->height_frames;
    int i;

    SDL_FreeSurface(image->surface);

    for(i = 0; i < num_frames; i++)
    {
        if(image->masks && image->masks[i])
            free(image->masks[i]);
    }
    
    free(image);

    return 1;

}

unsigned long long int bb_imagescollide(unsigned long long int image1_handle,long long int image1_x,long long int image1_y,unsigned long long int frame1,unsigned long long int image2_handle,long long int image2_x,long long int image2_y,unsigned long long int frame2)
{

    Image *image1 = (Image*)image1_handle;
    Image *image2 = (Image*)image2_handle;

    long long int offset_x = image1_x - image2_x;
    long long int offset_y = image1_y - image2_y;

    long long int sector_x1;
    long long int sector_x2;
    long long int sector_y1;
    long long int sector_y2;

    long long int remainder;

    long long int overlap_x;
    long long int overlap_y;
    long long int overlap_width;
    long long int overlap_height;

    long long int overlap_num_sectors_x;
    long long int overlap_num_sectors_y;

    long long int IMG1_WIDTH_SECTORS = image1->width / (8 * sizeof(unsigned long long int));
    if(!IMG1_WIDTH_SECTORS)
        IMG1_WIDTH_SECTORS++;
    long long int IMG1_HEIGHT_SECTORS = image1->height;

    long long int IMG2_WIDTH_SECTORS = image2->width / (8 * sizeof(unsigned long long int));;
    if(!IMG2_WIDTH_SECTORS)
        IMG2_WIDTH_SECTORS++;
    long long int IMG2_HEIGHT_SECTORS = image2->height;

    if(offset_x < 0)
    {
        overlap_x = (image2_x - image1_x);
        overlap_width = MIN(image1->width - overlap_x,image2->width);
    }
    else
    {
        overlap_x = (image2_x - image1_x);
        overlap_width = MIN(image2->width + overlap_x,image1->width);
    }

    if(offset_y < 0)
    {
        overlap_y = (image2_y - image1_y);
        overlap_height = MIN(image1->height - overlap_y,image2->height);
    }
    else
    {
        overlap_y = (image2_y - image1_y);
        overlap_height = MIN(image2->height + overlap_y,image1->height);
    }

    if(overlap_width < 0 || overlap_height < 0)
    {
        return 0;
    }

    if(overlap_x > image1->width || overlap_y > image1->height)
    {
        return 0;
    }

    if(offset_x < 0 && offset_y < 0)
    {
        sector_x1 = overlap_x / (8 * sizeof(unsigned long long int));
        sector_y1 = abs(overlap_y);
        sector_x2 = 0;
        sector_y2 = 0;

    }
    else if(offset_x < 0 && offset_y >= 0)
    {
        sector_x1 = abs(overlap_x / (long long int)(8 * sizeof(unsigned long long int)));
        sector_y1 = 0;

        sector_x2 = 0;
        sector_y2 = abs(overlap_y);
    }
    else if(offset_x >= 0 && offset_y < 0)
    {
        sector_x1 = 0;
        sector_y1 = abs(overlap_y);
        sector_x2 = abs(overlap_x / (long long int)(8 * sizeof(unsigned long long int)));
        sector_y2 = 0;
    }
    else
    {
        sector_x1 = 0;
        sector_y1 = 0;
        sector_x2 = abs(overlap_x / (long long int)(8 * sizeof(unsigned long long int)));
        sector_y2 = abs(overlap_y);
    }

    remainder = abs(overlap_x % (long long int)(8 * sizeof(unsigned long long int)));

    overlap_num_sectors_x = abs(overlap_width) / (((long long int)(8 * sizeof(unsigned long long int))));
    overlap_num_sectors_y = abs(overlap_height);

    if(remainder)
        overlap_num_sectors_x++;

    long long int i,j;
    long long int x,y;
    unsigned long long int fish,cats;

    if(IMG1_WIDTH_SECTORS >= IMG2_WIDTH_SECTORS)
    {
        for(y = 0; y < overlap_num_sectors_y; y++)
        {
            for(x = 0; x < overlap_num_sectors_x; x++)
            {
                if(x >= IMG2_WIDTH_SECTORS)
                {
                    i = sector_x1 + (x) + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                    j = sector_x2 + (x) - 1 + (sector_y2 + y) * IMG2_WIDTH_SECTORS;
                    fish = image1->masks[frame1][i];
                    cats = image2->masks[frame2][j] << ((8 * sizeof(unsigned long long int)) - remainder);
                    if(fish & cats)
                        return 1;

                }
                else
                {
                    i = sector_x1 + (x) + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                    j = sector_x2 + (x) + (sector_y2 + y) * IMG2_WIDTH_SECTORS;
                    fish = image1->masks[frame1][i];
                    if(overlap_x >= 0)
                        cats = image2->masks[frame2][j] >> remainder;
                    else
                        cats = image2->masks[frame2][j] << remainder;

                    if(remainder)
                    {
                        if(overlap_x >= 0)
                        {
                            if(x)
                            {
                                j = (sector_x2 + (x) - 1) + (sector_y2 + y) * IMG2_WIDTH_SECTORS;
                                cats |= image2->masks[frame2][j] << ((8 * sizeof(unsigned long long int)) - remainder);
                            }
                        }
                        else
                        {

                            if(x < overlap_num_sectors_x - 1)
                            {
                                j = (sector_x2 + (x) + 1) + (sector_y2 + y) * IMG2_WIDTH_SECTORS;
                                cats |= image2->masks[frame2][j] >> ((8 * sizeof(unsigned long long int)) - remainder);
                            }
                        }
                    }

                    if(fish & cats)
                    {
                        return 1;
                    }
                }
            }
        }
    }
    else
    {
        for(y = 0; y < overlap_num_sectors_y; y++)
        {
            for(x = 0; x < overlap_num_sectors_x; x++)
            {
                if(x >= IMG1_WIDTH_SECTORS)
                {

                    i = sector_x1 + (x) - 1 + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                    j = sector_x2 + (x) + (sector_y2 + y) * IMG2_WIDTH_SECTORS;
                    fish = image2->masks[frame2][j];
                    cats = image1->masks[frame1][i] << ((8 * sizeof(unsigned long long int)) - remainder);
                    if(fish & cats)
                        return 1;

                }
                else
                {
                    i = sector_x1 + (x) + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                    j = sector_x2 + (x) + (sector_y2 + y) * IMG2_WIDTH_SECTORS;

                    fish = image2->masks[frame2][j];
                    if(overlap_x < 0)
                        cats = image1->masks[frame1][i] >> remainder;
                    else
                        cats = image1->masks[frame1][i] << remainder;


                    if(remainder)
                    {
                        if(overlap_x < 0)
                        {
                            if(x)
                            {
                                i = sector_x1 + (x) - 1 + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                                cats |= image1->masks[frame1][i] << ((8 * sizeof(unsigned long long int)) - remainder);
                            }
                        }
                        else
                        {
                            if(x < overlap_num_sectors_x - 1)
                            {
                                i = sector_x1 + (x) + 1 + (sector_y1 + y) * IMG1_WIDTH_SECTORS;
                                cats |= image1->masks[frame1][i] >> ((8 * sizeof(unsigned long long int)) - remainder);
                            }
                        }
                    }

                    if(fish & cats)
                    {
                        return 1;
                    }
                }
            }
        }
    }

    return 0;

}

unsigned long long int bb_imagerectcollide(unsigned long long int image_handle,long long int image_x,long long int image_y,unsigned long long int frame,long long int rect_x,long long int rect_y,long long int rect_width,long long int rect_height)
{
    Image *rectangle = (Image*)malloc(sizeof(Image));
    unsigned long long int *rect_mask;
    long long int width,height,result;
    long long int x,y;
    long long int i;
    long long int remainder;
    unsigned long long int one = 1;
    unsigned long long int bits_less_one = 8 * sizeof(unsigned long long int) - 1;

    width = rect_width / (8 * sizeof(unsigned long long int)) + 1;
    remainder = rect_width % (8 * sizeof(unsigned long long int));

    height = rect_height;

    rect_mask = (unsigned long long int*)malloc(width * height * sizeof(unsigned long long int));
    memset(rect_mask,0,width * height * sizeof(unsigned long long int));

    for(y = 0; y < height; y++)
    {
        for(x = 0; x < width; x++)
        {
            if(x == width - 1)
            {
                for(i = 0; i < remainder; i++)
                {
                    unsigned long long int one = 1;
                    rect_mask[x + y * width] |= one << (bits_less_one - i);
                }
            }
            else
            {
                rect_mask[x + y * width] = 0xFFFFFFFFFFFFFFFF;
            }
        }
    }
    rectangle->masks = &rect_mask;
    rectangle->width = rect_width;
    rectangle->height = rect_height;

    result = bb_imagescollide(image_handle,image_x,image_y,frame,(unsigned long long int)rectangle,rect_x,rect_y,0);

    free(rect_mask);
    free(rectangle);

    return result;
}

unsigned long long int bb_maskimage(unsigned long long int image_handle,unsigned long long int red,unsigned long long int green,unsigned long long int blue)
{
    Image *image = (Image*)image_handle;
    unsigned long long int mask_color;
    
    if(SURFACE_BYTE_ORDER == 0x00FF0000)
        mask_color = red << 16 | green << 8 | blue;
    else
        mask_color = blue << 16 | green << 8 | red;
    long long int prev_mask_color = image->mask_color;
    mask_surface(image->surface,mask_color,prev_mask_color);
    image->mask_color = mask_color;
    image->masks = surface_to_masks(image->surface,image->width,image->height,mask_color);
    SDL_DestroyTexture(image->texture);
    image->texture = SDL_CreateTextureFromSurface(RENDERER,image->surface);
    return 1;
}

unsigned long long int **surface_to_masks(SDL_Surface *surface,int width,int height,unsigned long long int mask_color)
{
    unsigned long long int **masks;
    unsigned long long int *pixels;

    unsigned long long int width_sectors;
    unsigned long long int height_sectors;

    unsigned long long int width_frames;
    unsigned long long int height_frames;
    unsigned long long int num_frames;

    unsigned long long int frame,x,y,quotient,remainder;
    int value;
    int pixel_x;
    int pixel_y;

    width_sectors = width / (8 * sizeof(unsigned long long int));
    if(!width_sectors)
        width_sectors++;

    if(width % (8 * sizeof(unsigned long long int))&& width_sectors > 1)
        width_sectors++;

    width_frames = surface->w / width;
    height_frames = surface->h / height;
    num_frames = width_frames * height_frames;

    masks = (unsigned long long int**)malloc(num_frames * sizeof(unsigned long long int*));
    int xf,yf;

    for(frame = 0; frame < num_frames; frame++)
    {
        pixels = (unsigned long long int*)malloc(width_sectors * height * sizeof(unsigned long long int*));
        memset(pixels,0,width_sectors * height * sizeof(unsigned long long int*));

        yf = frame / width_frames * surface->w * height;
        xf = frame % width_frames * width;

        unsigned long long int z;
        for(y = 0; y < height; y++)
        {
            for(x = 0; x < width; x++)
            {
                value = ((int*)surface->pixels)[xf + x + yf + y * surface->w];

                if((value & 0x00FFFFFF) != mask_color)
                {
                    quotient = x / (8 * sizeof(unsigned long long int));
                    remainder = x % (8 * sizeof(unsigned long long int));
                    pixels[quotient + y * width_sectors] |= (0x8000000000000000 >> remainder);
                }

            }
        }

        masks[frame] = pixels;
    }

    return masks;


}

unsigned long long int mask_surface(SDL_Surface *surface,unsigned long long int mask_color,unsigned long long int prev_mask_color)
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
    return 1;
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

unsigned long long int bb_drawimage(unsigned long long int image_handle,long long int x,long long int y,unsigned long long int frame)
{
  Image *image = (Image*)image_handle;
  SDL_Rect dr,sr;
  SDL_Point p;
  
  int num_frames_x = image->surface->w / image->width;
  
  sr.x = image->width * (frame % num_frames_x);
  sr.y = image->height * (frame / num_frames_x);
  sr.w = image->width;
  sr.h = image->height;
  
  dr.x = (BB_ORIGIN_X + x) / BB_SCALE_X - image->handle_x;
  dr.y = (BB_ORIGIN_Y + y) / BB_SCALE_Y - image->handle_y;
  dr.w = image->width;
  dr.h = image->height;
  
  p.x = image->handle_x;
  p.y = image->handle_y;
  
  SDL_SetTextureAlphaMod(image->texture,BB_COLOR_ALPHA);
  SDL_RenderCopyEx(RENDERER,image->texture,&sr,&dr,BB_ORIENTATION,&p,SDL_FLIP_NONE);
}

unsigned long long int bb_drawimagerect(unsigned long long int image_handle,long long int x,long long int y,long long int rect_x1,long long int rect_y1,long long int rect_width,long long int rect_height,long long int frame)
{

  Image *image = (Image*)image_handle;
  SDL_Rect dr,sr;
  SDL_Point p;
  int num_frames_x = image->surface->w / image->width;
  
  sr.x = image->width * (frame % num_frames_x) + rect_x1;
  sr.y = image->height * (frame / num_frames_x) + rect_y1;
  sr.w = rect_width;
  sr.h = rect_height;
  
  dr.x = (BB_ORIGIN_X + x) / BB_SCALE_X - image->handle_x;
  dr.y = (BB_ORIGIN_Y + y) / BB_SCALE_Y - image->handle_y;
  dr.w = rect_width;
  dr.h = rect_height;
  
  p.x = image->handle_x;
  p.y = image->handle_y;
  
  SDL_SetTextureAlphaMod(image->texture,BB_COLOR_ALPHA);
  SDL_RenderCopyEx(RENDERER,image->texture,&sr,&dr,BB_ORIENTATION,&p,SDL_FLIP_NONE);
}

unsigned long long int bb_line(long long int x1,long long int y1,long long int x2,long long int y2)
{
    SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
    SDL_RenderDrawLine(RENDERER,x1 * BB_SCALE_X,y1 * BB_SCALE_Y,x2 * BB_SCALE_X,y2 * BB_SCALE_Y);
}

unsigned long long int bb_oval(long long int x,long long int y,long long int width,long long int height,unsigned long long int filled)
{
    if(!filled)
        bb_oval_hollow(x,y,width,height);
    else
        bb_oval_filled(x,y,width,height);
    return 1;
}

unsigned long long int bb_oval_hollow(long long int x,long long int y,long long int width,long long int height)
{
  SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
  aaellipseRGBA(RENDERER,(BB_ORIGIN_X + x) / BB_SCALE_X + width / 2,(BB_ORIGIN_Y + y) / BB_SCALE_Y + height / 2,0.5 * width,0.5 * height,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
}

unsigned long long int bb_oval_filled(long long int x,long long int y,long long int width,long long int height)
{
  SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
  filledEllipseRGBA(RENDERER,(BB_ORIGIN_X + x) / BB_SCALE_X + width / 2,(BB_ORIGIN_Y + y) / BB_SCALE_Y + height / 2,0.5 * width,0.5 * height,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
}

unsigned long long int bb_rect(long long int x,long long int y,long long int width,long long int height,unsigned long long int filled)
{
    if(!filled)
        bb_rect_hollow(x,y,width,height);
    else
        bb_rect_filled(x,y,width,height);
    return 1;
}

unsigned long long int bb_rect_filled(long long int x,long long int y,long long int width,long long int height)
{
  SDL_Rect r;
  //r.x = (BB_ORIGIN_X + x - width / 2) / BB_SCALE_X;
  //r.y = (BB_ORIGIN_Y + y - height / 2) / BB_SCALE_Y;
  r.x = (BB_ORIGIN_X + x) / BB_SCALE_X;
  r.y = (BB_ORIGIN_Y + y) / BB_SCALE_Y;
  r.w = width;
  r.h = height;
  SDL_SetRenderDrawBlendMode(RENDERER,SDL_BLENDMODE_BLEND);
  SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
  SDL_RenderDrawRect(RENDERER,&r);
  SDL_RenderFillRect(RENDERER,&r);
}

unsigned long long int bb_rect_hollow(long long int x,long long int y,long long int width,long long int height)
{
  SDL_Rect r;
  r.x = (BB_ORIGIN_X + x - width / 2) / BB_SCALE_X;
  r.y = (BB_ORIGIN_Y + y - height / 2) / BB_SCALE_Y;
  r.w = width;
  r.h = height;
  SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA); 
  SDL_RenderDrawRect(RENDERER,&r);
}

unsigned long long int bb_color(unsigned long long int red,unsigned long long int green,unsigned long long int blue, unsigned long long int alpha);
unsigned long long int bb_flip(unsigned long long int sync)
{
  SDL_RenderPresent(RENDERER);
  return 1;
}

SDL_Surface *flip_surface(SDL_Surface *surface)
{
    SDL_Surface *flipped = SDL_CreateRGBSurface(SDL_SWSURFACE, surface->w, surface->h, 24, 0x000000FF, 0x0000FF00, 0x00FF0000,0);

    int x,y,y2;
    unsigned char *pixels, *flipped_pixels;
    unsigned char r,g,b;

    pixels = (unsigned char*)surface->pixels;
    flipped_pixels = (unsigned char*)flipped->pixels;

    for(y = 0; y < surface->h; y++)
    {
        for(x = 0; x < surface->w; x++)
        {
            r = pixels[(x * 3 + 0) + y * 3 * surface->w ];
            g = pixels[(x * 3 + 1) + y * 3 * surface->w];
            b = pixels[(x * 3 + 2) + y * 3 * surface->w];

            y2 = surface->h - y - 1;
            flipped_pixels[(x * 3 + 0) + y2 * 3 * surface->w] = r;
            flipped_pixels[(x * 3 + 1) + y2 * 3 * surface->w] = g;
            flipped_pixels[(x * 3 + 2) + y2 * 3 * surface->w] = b;
        }
    }
    return flipped;
}

void bb_screenshot(char* name)
{
/*
    SDL_Surface * image = SDL_CreateRGBSurface(SDL_SWSURFACE, BB_GRAPHICS_WIDTH, BB_GRAPHICS_HEIGHT, 24, 0x000000FF, 0x0000FF00, 0x00FF0000,0);

    glReadBuffer(GL_FRONT);
    glReadPixels(0, 0, BB_GRAPHICS_WIDTH, BB_GRAPHICS_HEIGHT, GL_RGB,GL_UNSIGNED_BYTE, image->pixels);
    SDL_Surface *flipped = flip_surface(image);
    SDL_SaveBMP(flipped,name);
    SDL_FreeSurface(image);
    SDL_FreeSurface(flipped);
*/
}

unsigned long long int bb_clscolor(unsigned long long int red,unsigned long long int green,unsigned long long int blue, unsigned long long int alpha)
{
    BB_CLS_COLOR_RED = red;
    BB_CLS_COLOR_GREEN = green;
    BB_CLS_COLOR_BLUE = blue;
    BB_CLS_COLOR_ALPHA = alpha;
    return 0;
}

unsigned long long int bb_color(unsigned long long int red,unsigned long long int green,unsigned long long int blue, unsigned long long int alpha)
{
    BB_COLOR_RED = red;
    BB_COLOR_GREEN = green;
    BB_COLOR_BLUE = blue;
    //BB_COLOR_ALPHA = alpha;
    //SDL_SetRenderDrawColor(RENDERER,BB_COLOR_RED,BB_COLOR_GREEN,BB_COLOR_BLUE,BB_COLOR_ALPHA);
    return 0;
}
}

