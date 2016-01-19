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

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_mixer.h>
#include <GL/glew.h>
#include <GL/glext.h>
#include <SDL2/SDL_opengl.h>
#include <GL/glu.h>

#if WINDOWS==1
#include <windows.h>
#endif

#include <glib.h>

#if LINUX==1
char *NEW_LINE = "\n";
#elif WINDOWS==1
char *NEW_LINE = "\r\n";
#endif

extern long long int BB_KEYBOARD[];
extern long long int SDL_KEYBOARD[];

typedef struct
{
    long long int time;
    long long int frequency;
} Timer;

void init_scancode_translation();
void bb_init_libkoshka_core(unsigned long long int max_gosub_depth);
void bb_final_libkoshka_core();
void bb_fatal_error(char *msg);
 
void bb_init_libkoshka_mm(unsigned long long int max_gosub_depth)
{
    /*
    BB_MAX_GOSUB_DEPTH = max_gosub_depth;
    BB_GOSUB_STACK = malloc(max_gosub_depth * sizeof(unsigned long long int));
    memset(BB_GOSUB_STACK,0,sizeof(long long int) * BB_MAX_GOSUB_DEPTH);
    BB_GOSUB_STACK_POINTER = 0;
    
    BB_FATAL_ERROR_FILE_NAME = "";
    */
    bb_init_libkoshka_core(max_gosub_depth);
    
    memset(BB_KEYBOARD,0,sizeof(long long int) * SDL_NUM_SCANCODES);
    memset(SDL_KEYBOARD,0,sizeof(long long int) * SDL_NUM_SCANCODES);

    init_scancode_translation();
    
    if(SDL_Init(SDL_INIT_EVERYTHING))
    {
	char msg[256];
	sprintf(msg,"SDL initialisation error: %s\n",SDL_GetError());
	bb_fatal_error(msg);
    }
    
    if(TTF_Init())
    {
	char msg[256];
	sprintf(msg,"SDL_ttf initialisation error.\n");
	bb_fatal_error(msg);
    }
    
    if(Mix_OpenAudio(22050, MIX_DEFAULT_FORMAT, 2, 1024))
    {
	char msg[256];
	sprintf(msg,"SDL_Mixer initialisation error: %s.\n",Mix_GetError());
	bb_fatal_error(msg);
    }
    
    Mix_AllocateChannels(32);
    
}

void init_scancode_translation()
{
    BB_KEYBOARD[1] = SDL_SCANCODE_ESCAPE;
    BB_KEYBOARD[2] = SDL_SCANCODE_1;
    BB_KEYBOARD[3] = SDL_SCANCODE_2;
    BB_KEYBOARD[4] = SDL_SCANCODE_3;
    BB_KEYBOARD[5] = SDL_SCANCODE_4;
    BB_KEYBOARD[6] = SDL_SCANCODE_5;
    BB_KEYBOARD[7] = SDL_SCANCODE_6;
    BB_KEYBOARD[8] = SDL_SCANCODE_7;
    BB_KEYBOARD[9] = SDL_SCANCODE_8;
    BB_KEYBOARD[10] = SDL_SCANCODE_9;
    BB_KEYBOARD[11] = SDL_SCANCODE_0;
    BB_KEYBOARD[12] = SDL_SCANCODE_MINUS;
    BB_KEYBOARD[13] = SDL_SCANCODE_EQUALS;
    BB_KEYBOARD[14] = SDL_SCANCODE_BACKSPACE;
    BB_KEYBOARD[15] = SDL_SCANCODE_TAB;
    BB_KEYBOARD[16] = SDL_SCANCODE_Q;
    BB_KEYBOARD[17] = SDL_SCANCODE_W;
    BB_KEYBOARD[18] = SDL_SCANCODE_E;
    BB_KEYBOARD[19] = SDL_SCANCODE_R;
    BB_KEYBOARD[20] = SDL_SCANCODE_T;
    BB_KEYBOARD[21] = SDL_SCANCODE_Y;
    BB_KEYBOARD[22] = SDL_SCANCODE_U;
    BB_KEYBOARD[23] = SDL_SCANCODE_I;
    BB_KEYBOARD[24] = SDL_SCANCODE_O;
    BB_KEYBOARD[25] = SDL_SCANCODE_P;

    BB_KEYBOARD[26] = SDL_SCANCODE_LEFTBRACKET;
    BB_KEYBOARD[27] = SDL_SCANCODE_RIGHTBRACKET;

    BB_KEYBOARD[28] = SDL_SCANCODE_RETURN;
    BB_KEYBOARD[29] = SDL_SCANCODE_LCTRL;

    BB_KEYBOARD[30] = SDL_SCANCODE_A;
    BB_KEYBOARD[31] = SDL_SCANCODE_S;
    BB_KEYBOARD[32] = SDL_SCANCODE_D;
    BB_KEYBOARD[33] = SDL_SCANCODE_F;
    BB_KEYBOARD[34] = SDL_SCANCODE_G;
    BB_KEYBOARD[35] = SDL_SCANCODE_H;
    BB_KEYBOARD[36] = SDL_SCANCODE_J;
    BB_KEYBOARD[37] = SDL_SCANCODE_K;
    BB_KEYBOARD[38] = SDL_SCANCODE_L;

    BB_KEYBOARD[39] = SDL_SCANCODE_SEMICOLON;
    BB_KEYBOARD[40] = SDL_SCANCODE_APOSTROPHE;
    BB_KEYBOARD[41] = SDL_SCANCODE_GRAVE;
    BB_KEYBOARD[42] = SDL_SCANCODE_LSHIFT;
    BB_KEYBOARD[43] = SDL_SCANCODE_BACKSLASH;

    BB_KEYBOARD[44] = SDL_SCANCODE_Z;
    BB_KEYBOARD[45] = SDL_SCANCODE_X;
    BB_KEYBOARD[46] = SDL_SCANCODE_C;
    BB_KEYBOARD[47] = SDL_SCANCODE_V;
    BB_KEYBOARD[48] = SDL_SCANCODE_B;
    BB_KEYBOARD[49] = SDL_SCANCODE_N;
    BB_KEYBOARD[50] = SDL_SCANCODE_M;

    BB_KEYBOARD[51] = SDL_SCANCODE_COMMA;
    BB_KEYBOARD[52] = SDL_SCANCODE_PERIOD;
    BB_KEYBOARD[53] = SDL_SCANCODE_SLASH;
    BB_KEYBOARD[54] = SDL_SCANCODE_RSHIFT;
    BB_KEYBOARD[55] = SDL_SCANCODE_KP_MULTIPLY;

    BB_KEYBOARD[56] = SDL_SCANCODE_LALT;
    BB_KEYBOARD[57] = SDL_SCANCODE_SPACE;
    BB_KEYBOARD[58] = SDL_SCANCODE_CAPSLOCK;

    BB_KEYBOARD[59] = SDL_SCANCODE_F1;
    BB_KEYBOARD[60] = SDL_SCANCODE_F2;
    BB_KEYBOARD[61] = SDL_SCANCODE_F3;
    BB_KEYBOARD[62] = SDL_SCANCODE_F4;
    BB_KEYBOARD[63] = SDL_SCANCODE_F5;
    BB_KEYBOARD[64] = SDL_SCANCODE_F6;
    BB_KEYBOARD[65] = SDL_SCANCODE_F7;
    BB_KEYBOARD[66] = SDL_SCANCODE_F8;
    BB_KEYBOARD[67] = SDL_SCANCODE_F9;
    BB_KEYBOARD[68] = SDL_SCANCODE_F10;

    BB_KEYBOARD[69] = SDL_SCANCODE_NUMLOCKCLEAR;
    BB_KEYBOARD[70] = SDL_SCANCODE_SCROLLLOCK;
    BB_KEYBOARD[71] = SDL_SCANCODE_KP_7;
    BB_KEYBOARD[72] = SDL_SCANCODE_KP_8;
    BB_KEYBOARD[73] = SDL_SCANCODE_KP_9;

    BB_KEYBOARD[74] = SDL_SCANCODE_KP_MINUS;
    BB_KEYBOARD[75] = SDL_SCANCODE_KP_4;
    BB_KEYBOARD[76] = SDL_SCANCODE_KP_5;
    BB_KEYBOARD[77] = SDL_SCANCODE_KP_6;

    BB_KEYBOARD[78] = SDL_SCANCODE_KP_PLUS;
    BB_KEYBOARD[79] = SDL_SCANCODE_KP_1;
    BB_KEYBOARD[80] = SDL_SCANCODE_KP_2;
    BB_KEYBOARD[81] = SDL_SCANCODE_KP_3;
    BB_KEYBOARD[82] = SDL_SCANCODE_KP_0;

    BB_KEYBOARD[83] = SDL_SCANCODE_KP_PERIOD;

    BB_KEYBOARD[87] = SDL_SCANCODE_F11;
    BB_KEYBOARD[88] = SDL_SCANCODE_F12;
    BB_KEYBOARD[100] = SDL_SCANCODE_F13;
    BB_KEYBOARD[101] = SDL_SCANCODE_F14;
    BB_KEYBOARD[102] = SDL_SCANCODE_F15;

    BB_KEYBOARD[125] = SDL_SCANCODE_INTERNATIONAL3;

    BB_KEYBOARD[141] = SDL_SCANCODE_KP_EQUALS;

    BB_KEYBOARD[144] = SDL_SCANCODE_AUDIOPREV;
    BB_KEYBOARD[145] = SDL_SCANCODE_KP_AT;
    BB_KEYBOARD[146] = SDL_SCANCODE_KP_COLON;

    BB_KEYBOARD[149] = SDL_SCANCODE_STOP;

    BB_KEYBOARD[153] = SDL_SCANCODE_AUDIONEXT;
    
    BB_KEYBOARD[156] = SDL_SCANCODE_KP_ENTER;
    BB_KEYBOARD[157] = SDL_SCANCODE_RCTRL;

    BB_KEYBOARD[160] = SDL_SCANCODE_MUTE;
    BB_KEYBOARD[161] = SDL_SCANCODE_CALCULATOR;
    BB_KEYBOARD[162] = SDL_SCANCODE_AUDIOPLAY;

    BB_KEYBOARD[164] = SDL_SCANCODE_AUDIOSTOP;

    BB_KEYBOARD[174] = SDL_SCANCODE_VOLUMEDOWN;
    BB_KEYBOARD[176] = SDL_SCANCODE_VOLUMEUP;

    BB_KEYBOARD[178] = SDL_SCANCODE_WWW;
    BB_KEYBOARD[179] = SDL_SCANCODE_KP_COMMA;

    BB_KEYBOARD[181] = SDL_SCANCODE_KP_DIVIDE;

    BB_KEYBOARD[183] = SDL_SCANCODE_SYSREQ;
    BB_KEYBOARD[184] = SDL_SCANCODE_RALT;

    BB_KEYBOARD[197] = SDL_SCANCODE_PAUSE;

    BB_KEYBOARD[199] = SDL_SCANCODE_HOME;
    BB_KEYBOARD[200] = SDL_SCANCODE_UP;
    BB_KEYBOARD[201] = SDL_SCANCODE_PAGEUP;

    BB_KEYBOARD[203] = SDL_SCANCODE_LEFT;
    BB_KEYBOARD[205] = SDL_SCANCODE_RIGHT;

    BB_KEYBOARD[207] = SDL_SCANCODE_END;
    BB_KEYBOARD[208] = SDL_SCANCODE_DOWN;

    BB_KEYBOARD[210] = SDL_SCANCODE_INSERT;
    BB_KEYBOARD[211] = SDL_SCANCODE_DELETE;

    BB_KEYBOARD[219] = SDL_SCANCODE_LGUI;
    BB_KEYBOARD[220] = SDL_SCANCODE_RGUI;

    BB_KEYBOARD[222] = SDL_SCANCODE_POWER;
    BB_KEYBOARD[223] = SDL_SCANCODE_SLEEP;

    BB_KEYBOARD[227] = SDL_SCANCODE_SLEEP;

    BB_KEYBOARD[229] = SDL_SCANCODE_AC_SEARCH;
    BB_KEYBOARD[231] = SDL_SCANCODE_AC_REFRESH;
    BB_KEYBOARD[232] = SDL_SCANCODE_AC_STOP; 
    BB_KEYBOARD[233] = SDL_SCANCODE_AC_FORWARD;
    BB_KEYBOARD[234] = SDL_SCANCODE_AC_BACK;
    BB_KEYBOARD[235] = SDL_SCANCODE_COMPUTER;
    BB_KEYBOARD[236] = SDL_SCANCODE_MAIL;
    BB_KEYBOARD[237] = SDL_SCANCODE_MEDIASELECT; 
    
    
}

void bb_final_libkoshka_mm()
{
    Mix_CloseAudio();
    SDL_Quit();
    bb_final_libkoshka_core();
}

unsigned long long int bb_createtimer(unsigned long long int frequency)
{
    Timer *timer = (Timer*)malloc(sizeof(Timer));
    timer->frequency = frequency;
    timer->time = SDL_GetTicks();
    return (unsigned long long int)timer;
}

unsigned long long int bb_waittimer(unsigned long long int timer_handle)
{
    Timer *timer = (Timer*)timer_handle;
    long long int elapsed;
    long long int pings;

    do
    {
	elapsed = SDL_GetTicks() - timer->time;
	if(!(elapsed < (1000.0f / timer->frequency)))
	    break;
	else
	    SDL_Delay(0);
    } while(1);
	    
    pings = elapsed / ((long long int)(1000.0f / timer->frequency));
    timer->time += pings * ((long long int)(1000.0f / timer->frequency));
    return pings;
}

unsigned long long int bb_millisecs()
{
    return SDL_GetTicks();
}
