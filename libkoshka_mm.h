typedef struct
{
    unsigned long long int width,height;
    long long int handle_x,handle_y;
    GLuint texture;
    SDL_Surface *surface;
    GLuint mask;
} Image;

void bb_fatal_error(char *msg);
