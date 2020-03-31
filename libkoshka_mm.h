typedef struct
{
    long long int width,height;
    long long int width_frames,height_frames;
    long long int handle_x,handle_y;
    SDL_Surface *surface;
    SDL_Texture *texture;
    unsigned long long int mask_color;
    unsigned long long int **masks;
    unsigned long long int mask_width;
    unsigned long long int mask_height;
} Image;

void bb_fatal_error(char *msg);
