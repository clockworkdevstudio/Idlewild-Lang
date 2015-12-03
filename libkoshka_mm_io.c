#include <SDL2/SDL.h>

SDL_Event EVENT;
long long int SDL_KEYBOARD[SDL_NUM_SCANCODES];
long long int BB_KEYBOARD[SDL_NUM_SCANCODES];
int BB_KEY_PRESSED = 1;
int BB_KEY_RELEASED = 2;

void bb_processevents()
{
    while(SDL_PollEvent(&EVENT))
    {
	switch(EVENT.type)
	{
	case SDL_KEYDOWN:
	{
	    SDL_KEYBOARD[EVENT.key.keysym.scancode] = BB_KEY_PRESSED;
	    break;
	}
	case SDL_KEYUP:
	{
	    SDL_KEYBOARD[EVENT.key.keysym.scancode] = BB_KEY_RELEASED;
	    break;
	}
	}
    }
    return;
}

unsigned long long int bb_keydown(unsigned long long int scancode)
{
    bb_processevents();	
    if(SDL_KEYBOARD[BB_KEYBOARD[scancode]] == BB_KEY_PRESSED)
	return 1;
    else
	return 0;
}

unsigned long long int bb_keyhit(unsigned long long int scancode)
{
    bb_processevents();
    if(SDL_KEYBOARD[BB_KEYBOARD[scancode]] == BB_KEY_RELEASED)
    {
	SDL_KEYBOARD[BB_KEYBOARD[scancode]] = 0;
	return 1;
    }
    else
	return 0;
}
