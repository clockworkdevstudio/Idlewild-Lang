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

#include <stdio.h>
#include <SDL2/SDL_ttf.h>
#include "libkoshka_mm.h"

unsigned long long int bb_drawimage(unsigned long long int image_handle,long long int x,long long int y,unsigned long long int frame);

extern unsigned long long int BB_COLOR_RED;
extern unsigned long long int BB_COLOR_GREEN;
extern unsigned long long int BB_COLOR_BLUE;
extern unsigned long long int BB_COLOR_ALPHA;
extern SDL_Renderer *RENDERER;

unsigned long long int draw_text(unsigned long long int image_handle,long long int x,long long int y);

TTF_Font *BB_CURRENT_FONT;

unsigned long long int bb_loadfont(char *font_name,unsigned long long int height,unsigned long long int bold,unsigned long long int italic,unsigned long long int underlined)
{
    TTF_Font *font = TTF_OpenFont(font_name,height);
    return (unsigned long long int)font;
}

unsigned long long int bb_setfont(unsigned long long int font_handle)
{
    BB_CURRENT_FONT = (TTF_Font*)font_handle;
    return 0;
}

unsigned long long int bb_text(long long int x,long long int y,char *text,unsigned long long int center_x,unsigned long long int center_y)
{
    SDL_Surface *surface;
    SDL_Color color;
    Image *image;

    color.r = BB_COLOR_BLUE;
    color.g = BB_COLOR_GREEN;
    color.b = BB_COLOR_RED;
    color.a = BB_COLOR_ALPHA;

    surface = TTF_RenderUTF8_Blended(BB_CURRENT_FONT,text,color);
    
    image = (Image*)malloc(sizeof(Image));
    image->surface = surface;
    image->texture = SDL_CreateTextureFromSurface(RENDERER,image->surface);
    image->width = surface->w;
    image->height = surface->h;
    image->width_frames = 1;
    image->height_frames = 1;
    image->masks = 0;
    
    if(center_x)
    {
        image->handle_x = image->width / 2;
    }
    else
    {
        image->handle_x = 0;
    }

    if(center_y)
    {
        image->handle_y = image->height / 2;
    }
    else
    {
        image->handle_y = 0;
    }

    bb_drawimage((unsigned long long)image,x,y,0);

    SDL_DestroyTexture(image->texture);
    SDL_FreeSurface(image->surface);
    free(image);
    return 1;
}

unsigned long long int bb_stringwidth(char *text)
{
    int width;
    TTF_SizeUTF8(BB_CURRENT_FONT,text,&width,NULL);
    return width;
}

unsigned long long int bb_stringheight(char *text)
{
    int height;
    TTF_SizeUTF8(BB_CURRENT_FONT,text,NULL,&height);
    return height;
}
