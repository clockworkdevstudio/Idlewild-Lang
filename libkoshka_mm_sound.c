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

#include <SDL2/SDL_mixer.h>
#include <stdlib.h>

typedef struct
{
    unsigned long long int pitch;
    float volume;
    float pan;
    int loop;
    Mix_Chunk *sample;
} Sound;

unsigned long long int bb_loadsound(char *name)
{
    Mix_Chunk *sample;
    Sound *sound;
    sample = (Mix_Chunk*)Mix_LoadWAV(name);
    if(!sample)
	return 0;
    sound = (Sound*)malloc(sizeof(Sound));
    sound->pitch = 1;
    sound->volume = 0.5f;
    sound->pan = 0.0f;
    sound->loop = 0;
    sound->sample = sample;
    return (unsigned long long int)sound;
}

unsigned long long int bb_loopsound(unsigned long long int sound_handle)
{
    Sound *sound;
    sound = (Sound*)sound_handle;
    sound->loop = -1;
}

long long int bb_playsound(unsigned long long int sound_handle)
{
    Sound *sound;
    sound = (Sound*)sound_handle;
    int result;
    result = Mix_PlayChannel(-1,sound->sample,sound->loop);
    return (unsigned long long int)result;
}

unsigned long long int bb_stopchannel(long long int channel)
{
    Mix_HaltChannel(channel);
}

unsigned long long int bb_channelplaying(long long int channel)
{
    if(Mix_Playing(channel))
	return 1;
    else
	return 0;
}
