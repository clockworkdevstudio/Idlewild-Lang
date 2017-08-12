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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#if WINDOWS==1

#include <windows.h>
#include <mbstring.h>
#include <versionhelpers.h>
#include <malloc.h>
#include <glib.h>

#elif LINUX==1

#include <malloc.h>
#include <glib.h>

#elif MAC_OS==1

#import <Foundation/Foundation.h>
#include <sys/queue.h>
#include "it_quacks_like_glib.h"
#endif

#if LINUX==1 || MAC_OS==1
char *NEW_LINE = "\n";
#elif WINDOWS==1
char *NEW_LINE = "\r\n";
#endif

#define BB_INPUT_MAX 256
#define BB_ARRAY_MAX_DIMENSIONALITY 16
#define BB_SIZE_TEMP_STRING 256
#define BB_MAX_FOR_EACH_STACK 16

unsigned long long int BB_DOUBLE_PRINT_PRECISION = 6;
long long int *BB_DATA_POINTER;
double PI = 3.14159265358979323;

#define BB_DATA_TYPE_INT 1
#define BB_DATA_TYPE_FLOAT 2
#define BB_DATA_TYPE_STRING 3
#define BB_OUT_OF_DATA -1

typedef unsigned long long int BB_RESOURCE_HANDLE;

typedef struct
{
    unsigned long long int size;
    unsigned char *data;
} Bank;

typedef struct
{
    GList *first;
    GList *last;
    GList  *cached;
    unsigned long long int element_size;
    unsigned long long int *string_offsets;
    unsigned long long int **for_each_stack[BB_MAX_FOR_EACH_STACK];
    unsigned long long int for_each_index;
} LinkedList;

char BB_TEMP_STRING_BUFFER[BB_SIZE_TEMP_STRING];
unsigned long long int *BB_GOSUB_STACK;
unsigned long long int BB_GOSUB_STACK_POINTER;
unsigned long long int BB_MAX_GOSUB_DEPTH;
char *BB_FATAL_ERROR_FILE_NAME;
int BB_FATAL_ERROR_LINE_NUMBER;
int BB_FATAL_ERROR_CHARACTER_NUMBER;
double DPI_SCALE;

void bb_deallocate_array(unsigned long long int *array,void(*final_func)(void*),unsigned long long int dimensionality);
void bb_fatal_error(char *msg);
void bb_free_strings(unsigned long long int **address,unsigned long long int *offsets);
void bb_delete_each(LinkedList *list);

void bb_init_libkoshka_core(unsigned long long int max_gosub_depth)
{
    BB_MAX_GOSUB_DEPTH = max_gosub_depth;
    BB_GOSUB_STACK = malloc(max_gosub_depth * sizeof(unsigned long long int));
    BB_DATA_POINTER = 0;
    memset(BB_GOSUB_STACK,0,sizeof(long long int) * BB_MAX_GOSUB_DEPTH);
    BB_GOSUB_STACK_POINTER = 0;
    BB_FATAL_ERROR_FILE_NAME = "";
}

void bb_final_libkoshka_core()
{
    free(BB_GOSUB_STACK);
}

#if 0

HMONITOR GetPrimaryMonitorHandle()
{
    const POINT ptZero = { 0, 0 };
    return MonitorFromPoint(ptZero, MONITOR_DEFAULTTOPRIMARY);
}

void dpiHack(void)
{

    UINT x,y;
    HMODULE shcore = LoadLibraryA("C:\\Windows\\System32\\SHCore.dll");
    unsigned long long int(*setProcessDpiAwareness)(unsigned long long int);
    setProcessDpiAwareness = (unsigned long long int(*)(unsigned long long int))GetProcAddress(shcore,"SetProcessDpiAwareness");
    setProcessDpiAwareness(2);
    HRESULT WINAPI(*getDpiForMonitor)(HMONITOR,UINT,UINT*,UINT*);
    getDpiForMonitor = (HRESULT WINAPI(*)(HMONITOR,UINT,UINT*,UINT*))GetProcAddress(shcore,"GetDpiForMonitor");
    getDpiForMonitor((HMONITOR)GetPrimaryMonitorHandle(),0,&x,&y);
    DPI_SCALE = x / 96.0;
    FreeLibrary(shcore);
}
#else
void dpiHack(void)
{
    DPI_SCALE = 1.0;
}
#endif

void bb_init_gosub(unsigned long long int return_address)
{
    if(BB_GOSUB_STACK_POINTER == BB_MAX_GOSUB_DEPTH)
    {
        char msg[256];
        sprintf(msg,"Gosub stack too large (nested Gosubs exceeded %lld; recompile program with option --max-gosub-depth <depth> to address this error).",BB_MAX_GOSUB_DEPTH);
        bb_fatal_error(msg);
    }
    BB_GOSUB_STACK[BB_GOSUB_STACK_POINTER] = return_address;
    BB_GOSUB_STACK_POINTER++;
}

unsigned long long int bb_final_gosub()
{
    BB_GOSUB_STACK_POINTER--;
    if(BB_GOSUB_STACK_POINTER == -1)
        bb_fatal_error("Return without Gosub.");
    return BB_GOSUB_STACK[BB_GOSUB_STACK_POINTER];
}

void bb_anticipate_fatal_error(char *file_name,long long int line_number,long long int character_number)
{
    BB_FATAL_ERROR_FILE_NAME = file_name;
    BB_FATAL_ERROR_LINE_NUMBER = line_number;
    BB_FATAL_ERROR_CHARACTER_NUMBER = character_number;
}

void bb_fatal_error(char *msg)
{
    if(!strcmp("",BB_FATAL_ERROR_FILE_NAME))
        printf("%s For detailed diagnostic information recompile program with --debug option.%s",msg,NEW_LINE);
    else
        printf("%s: %d:%d: %s%s",BB_FATAL_ERROR_FILE_NAME,BB_FATAL_ERROR_LINE_NUMBER,BB_FATAL_ERROR_CHARACTER_NUMBER,msg,NEW_LINE);
    exit(1);
}

void bb_test_function(char *a,char *b,char *c, char *d,char *e,char *f,char *g)
{
    printf("%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",a,NEW_LINE,b,NEW_LINE,c,NEW_LINE,d,NEW_LINE,e,NEW_LINE,f,NEW_LINE,g,NEW_LINE,NEW_LINE);
}

int bb_test_function_int2(long long int i1,long long int i2)
{
    printf("%lld %lld%s",i1,i2,NEW_LINE);
    return 0;
}

int bb_test_function_int7(long long int i1,long long int i2,long long int i3,long long int i4,long long int i5,long long int i6,long long int i7)
{
    printf("%lld %lld %lld %lld %lld %lld %lld%s",i1,i2,i3,i4,i5,i6,i7,NEW_LINE);
    return 0;
}

int bb_test_function_int8(long long int i1,long long int i2,long long int i3,long long int i4,long long int i5,long long int i6,long long int i7,long long int i8)
{
    printf("%lld %lld %lld %lld %lld %lld %lld %lld%s",i1,i2,i3,i4,i5,i6,i7,i8,NEW_LINE);
    return 0;
}

double bb_test_function2(double f1,double f2)
{
    printf("%f + %f = %f%s",f1,f2,f1 + f2,NEW_LINE);
    return f1 + f2;
}

double bb_test_function_float1(double f1)
{
    printf("%f%s",f1,NEW_LINE);
    return f1;
}

int bb_test_function_float10(double f1,double f2,double f3,double f4,double f5,double f6,double f7,double f8,double f9,double f10)
{
    printf("%f%s%f%s%f%s%f%s%f%s%f%s%f%s%f%s%f%s%f%s",f1,NEW_LINE,f2,NEW_LINE,f3,NEW_LINE,f4,NEW_LINE,f5,NEW_LINE,f6,NEW_LINE,f7,NEW_LINE,f8,NEW_LINE,f9,NEW_LINE,f10,NEW_LINE);
    return 0;
}

int bb_test_function_float9(double f1,double f2,double f3,double f4,double f5,double f6,double f7,double f8,double f9)
{
    printf("%f%s%f%s%f%s%f%s%f%s%f%s%f%s%f%s%f%s",f1,NEW_LINE,f2,NEW_LINE,f3,NEW_LINE,f4,NEW_LINE,f5,NEW_LINE,f6,NEW_LINE,f7,NEW_LINE,f8,NEW_LINE,f9,NEW_LINE);
    return 0;
}

int bb_test_function_string1(char *s1)
{
    printf("%s%s",s1,NEW_LINE);
    return 0;
}

int bb_test_function_string2(char *s1,char *s2)
{
    printf("%s %s%s",s1,s2,NEW_LINE);
    return 0;
}

BB_RESOURCE_HANDLE bb_createbank(unsigned long long int size)
{
    Bank *bank;
    bank = malloc(sizeof(Bank));
    bank->size = size;
    bank->data = malloc(size);
    if(bank->data == 0)
    {
        free(bank);
        return 0;
    }
    memset(bank->data,0,size);
    return (BB_RESOURCE_HANDLE)bank;
}

unsigned long long int bb_freebank(BB_RESOURCE_HANDLE handle)
{
    free(((Bank*)handle)->data);
    free((Bank*)handle);
    return 0;
}

unsigned long long int bb_banksize(BB_RESOURCE_HANDLE handle)
{
    return ((Bank*)handle)->size;
}

unsigned long long int bb_resizebank(BB_RESOURCE_HANDLE handle,unsigned long long int new_size)
{
    Bank* bank = (Bank*)handle;
    void *new_data = malloc(new_size);
    if(new_data == 0)
        return 0;

    if(new_size > bank->size)
    {
        memcpy(new_data,bank->data,bank->size);
        memset(new_data + bank->size,0,new_size - bank->size);
    }
    else
    {
        memcpy(new_data,bank->data,new_size);
    }
    free(bank->data);
    bank->data = new_data;
    bank->size = new_size;
    return 1;
}

unsigned long long int bb_copybank(BB_RESOURCE_HANDLE handle1,unsigned long long int offset1,BB_RESOURCE_HANDLE handle2,unsigned long long int offset2,unsigned long long int num_bytes)
{
    Bank* bank1 = (Bank*)handle1;
    Bank* bank2 = (Bank*)handle2;

    if(offset1 + num_bytes > bank1->size)
        bb_fatal_error("CopyBank source address out of range.");

    if(offset2 + num_bytes > bank2->size)
        bb_fatal_error("CopyBank destination address out of range.");

    if(bank1 == bank2)
        memmove(bank2->data + offset2,bank1->data + offset1,num_bytes);
    else
        memcpy(bank2->data + offset2,bank1->data + offset1,num_bytes);

    return 1;
}

unsigned long long int bb_peekbyte(BB_RESOURCE_HANDLE handle,unsigned long long int offset)
{
    Bank *bank = (Bank*)handle;
    if(offset >= bank->size)
        bb_fatal_error("PokeByte address out of range.");
    return (unsigned long long int)(((unsigned char*)bank->data)[offset]);
}

unsigned long long int bb_pokebyte(BB_RESOURCE_HANDLE handle,unsigned long long int offset,unsigned long long int value)
{
    Bank *bank = (Bank*)handle;
    if(offset >= bank->size)
        bb_fatal_error("PokeByte address out of range.");
    ((unsigned char*)bank->data)[offset] = value;
    return 0;
}

unsigned long long int bb_peekshort(BB_RESOURCE_HANDLE handle,unsigned long long int offset)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 2)
        bb_fatal_error("PeekShort address out of range.");
    return (unsigned long long int)*((unsigned short*)(bank->data + offset));
}

unsigned long long int bb_pokeshort(BB_RESOURCE_HANDLE handle,unsigned long long int offset,unsigned long long int value)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 2)
        bb_fatal_error("PokeShort address out of range.");
    *((unsigned short*)(bank->data + offset)) = value;
    return 0;
}

unsigned long long int bb_peekint(BB_RESOURCE_HANDLE handle,unsigned long long int offset)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 4)
        bb_fatal_error("PeekInt address out of range.");
    return (unsigned long long int)*((unsigned int*)(bank->data + offset));
}

unsigned long long int bb_pokeint(BB_RESOURCE_HANDLE handle,unsigned long long int offset,unsigned long long int value)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 4)
        bb_fatal_error("PokeInt address out of range.");
    *((unsigned int*)(bank->data + offset)) = value;
    return 0;
}

unsigned long long int bb_peeklong(BB_RESOURCE_HANDLE handle,unsigned long long int offset)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 8)
        bb_fatal_error("PeekLong address out of range.");
    return *((unsigned long long int*)(bank->data + offset));
}

unsigned long long int bb_pokelong(BB_RESOURCE_HANDLE handle,unsigned long long int offset,unsigned long long int value)
{
    Bank *bank = (Bank*)handle;
    if(offset > bank->size - 8)
        bb_fatal_error("PokeLong address out of range.");
    *((unsigned long long int*)(bank->data + offset)) = value;
    return 0;
}

double bb_sin(double angle)
{
    return sin(angle / 360.0f * 2.0f * PI);
}

double bb_cos(double angle)
{
    return cos(angle / 360.0f * 2.0f * PI);
}

double bb_atan2(double y,double x)
{
    return (atan2(y,x) / (2.0f * PI)) * 360.0;
}

double bb_atan(double angle)
{
    return atan(angle);
}

double bb_tan(double angle)
{
    return tan(angle);
}

double bb_sgn(double value)
{
    if(value > 0.0f)
        return 1.0f;
    else if(value < 0.0f)
        return -1.0f;
    else
        return 0.0f;
}


double bb_exp(double value)
{
    return exp(value);
}

double bb_log(double value)
{
    return log(value);
}

double bb_sqr(double value)
{
    return sqrt(value);
}

unsigned long long int bb_seedrnd(unsigned long long int seed)
{
    srand(seed);
    return 0;
}

double bb_rnd(double lower_bound,double upper_bound)
{
    double difference = upper_bound - lower_bound;
    return (((double)rand()) / ((double)RAND_MAX)) * difference + lower_bound;
}

long long int bb_rand(long long int lower_bound,long long int upper_bound)
{
    long long int result;
    long long int quotient;

    if ((upper_bound - 1) == RAND_MAX)
        return rand();

    quotient = RAND_MAX / upper_bound;

    quotient *= upper_bound;

    while ((result = rand()) >= quotient);

    return lower_bound + result % upper_bound;
}

void bb_free_string(char *string)
{
    if(string && strlen(string) > 0)
    {
        free(string);
    }
}

char *bb_duplicate_string(char *string)
{
    char *new_string;
    new_string = malloc(strlen(string) + 1);
    return strcpy(new_string,string);
}

void bb_copy_string(char **dest,char *source)
{
    bb_free_string(*dest);
    if(strlen(source) == 0)
    {
        *dest = "";
    }
    else
    {
        *dest = malloc(strlen(source) + 1);
        strcpy(*dest,source);
    }
}

char *bb_concatenate_strings(char *string_left,char *string_right)
{
    int length_left;
    int length_right;
    char *new_string = "";

    length_left = strlen(string_left);
    length_right = strlen(string_right);
    if(length_right + length_left)
    {
        new_string = malloc(length_left + length_right + 1);
        strcpy(new_string,string_left);
        strcat(new_string,string_right);
    }
    return new_string;
}

#if LINUX==1
int bb_compare_strings(char *string_left,char *string_right)
{
    return g_utf8_collate(string_left,string_right);
}
#elif MAC_OS==1
int bb_compare_strings(char *string_left,char *string_right)
{
    @autoreleasepool
    {
        NSMutableString *l = [[NSMutableString alloc] initWithUTF8String:string_left];
        NSMutableString *r = [[NSMutableString alloc] initWithUTF8String:string_right];
        if([l compare: r] == NSOrderedAscending)
        {
            return -1;            
        }
        else if([l compare: r] == NSOrderedDescending)
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}
        
#elif WINDOWS==1
int bb_compare_strings(char *string_left,char *string_right)
{
    return _mbscmp(string_left,string_right);
}
#endif

char *bb_convert_int_to_string(long long int int_input)
{
    char *string;
    sprintf(BB_TEMP_STRING_BUFFER,"%lld",int_input);
    string = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
    return strcpy(string,BB_TEMP_STRING_BUFFER);
}

char *bb_convert_pointer_to_string(long long int pointer_input)
{
    char *string;
    sprintf(BB_TEMP_STRING_BUFFER,"$%.16llX",pointer_input);
    string = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
    return strcpy(string,BB_TEMP_STRING_BUFFER);
}

void bb_setprecision(unsigned long long int precision)
{
    BB_DOUBLE_PRINT_PRECISION = precision;
}

char *bb_convert_float_to_string(double float_input)
{
    unsigned long long int precision = BB_DOUBLE_PRINT_PRECISION > 99 ? 99 : BB_DOUBLE_PRINT_PRECISION;
    char *string;
    BB_TEMP_STRING_BUFFER[0] = 0;
    char format[6];
    if(BB_DOUBLE_PRINT_PRECISION > 9)
    {
        sprintf(format,"%%.%lldf",precision);
    }
    else
    {
        sprintf(format,"%%.%lldf",precision);
        format[5] = '\0';
    }
    sprintf(BB_TEMP_STRING_BUFFER,format,float_input);
    string = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
    return strcpy(string,BB_TEMP_STRING_BUFFER);
}

#if LINUX==1

long long int bb_len(char *string);

char *bb_mid(char *string,long long int offset,long long int num_characters)
{
    long long int length;
    offset--;
    if(!g_utf8_validate(string,-1,NULL))
        return "";
    length = g_utf8_strlen(string,-1);

    if(num_characters <= 0 || length == 0)
        return "";
    if(offset + num_characters > length)
    {
        if(offset == length)
        {
            return "";
        }
        return (char*)g_utf8_substring(string,offset,length);
    }
    else
    {
        return (char*)g_utf8_substring(string,offset,offset + num_characters);
    }
}

char *bb_left(char *string,long long int num_characters)
{
    long long int length;
    if(!g_utf8_validate(string,-1,NULL))
        return "";
    length = g_utf8_strlen(string,-1);
    if(length == 0)
        return "";
    if(num_characters <= 0)
        return "";
    if(length <= num_characters)
    {
        return (char*)g_utf8_substring(string,0,length);
    }
    else
    {
        return (char*)g_utf8_substring(string,0,num_characters);
    }
}

char *bb_right(char *string,long long int num_characters)
{
    long long int length;
    if(!g_utf8_validate(string,-1,NULL))
        return "";
    length = g_utf8_strlen(string,-1);
    if(length == 0)
        return "";
    if(num_characters <= 0)
        return "";
    if(length <= num_characters)
    {
        return (char*)g_utf8_substring(string,0,length);
    }
    else
    {
        return (char*)g_utf8_substring(string,length - num_characters,length);
    }
}

long long int bb_len(char *string)
{
    return (long long int)g_utf8_strlen(string,-1);
}

long long int bb_uni(char *string)
{
    return (long long int)g_utf8_get_char(string);
}

char *bb_chr(long long int value)
{
    return (char*)g_ucs4_to_utf8((gunichar*)&value,1,NULL,NULL,NULL);
}

char *bb_upper(char *string)
{
    gunichar *ucs4_input, *ucs4_output;
    glong items_read;
    glong items_written;
    int length;
    int i;
    char *result;

    ucs4_input = g_utf8_to_ucs4(string,-1,&items_read,&items_written,NULL);
    length = items_written;
    ucs4_output = malloc(length * sizeof(gunichar));
    for(i = 0; i < length; i++)
    {
        ucs4_output[i] = g_unichar_toupper(ucs4_input[i]);
    }
    result = g_ucs4_to_utf8(ucs4_output,length,&items_read,&items_written,NULL);
    free(ucs4_input);
    free(ucs4_output);
    return result;
}

char *bb_lower(char *string)
{
    gunichar *ucs4_input, *ucs4_output;
    glong items_read;
    glong items_written;
    int length;
    int i;
    char *result;
    if(strlen(string) == 0) return "";
    ucs4_input = g_utf8_to_ucs4(string,-1,&items_read,&items_written,NULL);
    length = items_written;
    ucs4_output = malloc(length * sizeof(gunichar));
    for(i = 0; i < length; i++)
    {
        ucs4_output[i] = g_unichar_tolower(ucs4_input[i]);
    }
    result = g_ucs4_to_utf8(ucs4_output,length,&items_read,&items_written,NULL);
    free(ucs4_input);
    free(ucs4_output);
    return result;
}
#elif MAC_OS==1

void codepoint_to_surrogate_pair(unsigned long long int codepoint,unsigned short *lead_surrogate,unsigned short *trail_surrogate)
{
    codepoint -= 0x10000;
    *lead_surrogate = ((codepoint >> 10) & 0x3FF) + 0xD800;
    *trail_surrogate = (codepoint & 0x3FF) + 0xDC00;
    return;
}

unsigned long long int surrogate_pair_to_codepoint(unsigned short lead_surrogate,unsigned short trail_surrogate)
{
    unsigned short unbiased_lead = lead_surrogate - 0xD800;
    unsigned short unbiased_trail = trail_surrogate - 0xDC00;
    unsigned long long int result = ((unbiased_lead << 10) | unbiased_trail) + 0x010000;
    return result;
}

long long int bb_len(char *string)
{
    @autoreleasepool
    {
        NSString *s = [[NSString alloc] initWithUTF8String:string];
        NSRange r;
        int i = 0,c = 0;

        while(i < [s length])
        {
            r = [s rangeOfComposedCharacterSequenceAtIndex:i];
            if(r.length == 1)
                i++;
            else
                i += 2;
            c++;

        }
        return c;
    }
}

long long int bb_mid(char *string,long long int offset,long long int num_characters)
{
    @autoreleasepool
    {
        offset--;
        NSMutableString *s = [[NSMutableString alloc] initWithUTF8String:string];
        NSRange range;
        NSString *substring;
        char *UTF8, *result;

        int is = 0,ie = 0,c = 0;
        int length = bb_len(string);

        if(num_characters <= 0 || length == 0)
            return (long long int)"";

        while(is < [s length] && c < offset)
        {
            range = [s rangeOfComposedCharacterSequenceAtIndex:is];
            if(range.length == 1)
                is++;
            else
                is += 2;
            c++;
        }

        ie = is;

        while(ie < [s length] && c < offset + num_characters && c < length)
        {
            range = [s rangeOfComposedCharacterSequenceAtIndex:ie];
            if(range.length == 1)
                ie++;
            else
                ie += 2;
            c++;
        }

        range.location = is;
        range.length = ie - is;
        substring = [s substringWithRange:range];
        UTF8 = (char*)[substring UTF8String];
        result = malloc(strlen(UTF8) + 1);
        strcpy(result,UTF8);
        return (long long int)result;
    }
}

char *bb_left(char *string,long long int num_characters)
{
    @autoreleasepool
    {
        NSMutableString *s = [[NSMutableString alloc] initWithUTF8String:string];
        NSRange range;
        NSString *substring;
        char *UTF8, *result;

        int ie = 0,c = 0;
        int length = bb_len(string);

        if(num_characters <= 0 || length == 0)
            return "";

        while(ie < [s length] && c < num_characters)
        {
            range = [s rangeOfComposedCharacterSequenceAtIndex:ie];
            if(range.length == 1)
                ie++;
            else
                ie += 2;
            c++;
        }

        range.location = 0;
        range.length = ie;
        substring = [s substringWithRange:range];
        UTF8 = (char*)[substring UTF8String];
        result = malloc(strlen(UTF8) + 1);
        strcpy(result,UTF8);
        return result;
    }
}

long long int bb_right(char *string,long long int num_characters)
{
    @autoreleasepool
    {
        NSMutableString *s = [[NSMutableString alloc] initWithUTF8String:string];
        NSRange range;
        NSString *substring;
        char *UTF8, *result;

        int is = 0,ie = 0,c = 0;
        int length = bb_len(string);

        if(num_characters <= 0 || length == 0)
            return (long long int)"";

        while(is < [s length] && c < length - num_characters)
        {
            range = [s rangeOfComposedCharacterSequenceAtIndex:is];
            if(range.length == 1)
                is++;
            else
                is += 2;
            c++;
        }

        ie = is;

        while(ie < [s length] && c < length)
        {
            range = [s rangeOfComposedCharacterSequenceAtIndex:ie];
            if(range.length == 1)
                ie++;
            else
                ie += 2;
            c++;
        }

        range.location = is;
        range.length = ie - is;
        substring = [s substringWithRange:range];
        UTF8 = (char*)[substring UTF8String];
        result = malloc(strlen(UTF8) + 1);
        strcpy(result,UTF8);
        return (long long int)result;
    }
}

char *bb_chr(unsigned long long int value)
{
    @autoreleasepool
    {
        unsigned short lead_surrogate;
        unsigned short trail_surrogate;
        unsigned short utf16_string[3];
        char *result;
        int size;
        NSString *apple_utf16_string;
        char *utf8_string;

        if(value > 0xFFFF)
        {
            codepoint_to_surrogate_pair(value,&lead_surrogate,&trail_surrogate);
            utf16_string[0] = lead_surrogate;
            utf16_string[1] = trail_surrogate;
            utf16_string[2] = 0;
            apple_utf16_string = [[NSString alloc] initWithCharacters:(unichar*)utf16_string length:3];
        }
        else
        {
            utf16_string[0] = value;
            utf16_string[1] = 0;
            apple_utf16_string = [[NSString alloc] initWithCharacters:(unichar*)utf16_string length:2];
        }

        utf8_string = (char*)[apple_utf16_string UTF8String];
        size = strlen(utf8_string) + 1;
        result = malloc(size);
        strcpy(result,utf8_string);
        return (char*)result;
    }
}

unsigned long long int bb_uni(char *string)
{
    @autoreleasepool
    {
        NSString *s;
        unsigned short *utf8_string;
        unsigned long long int result;
        NSRange composed;

        s = [[NSString alloc] initWithUTF8String:string];
        composed = [s rangeOfComposedCharacterSequenceAtIndex:0];
        if (composed.length == 2)
        {
            result = surrogate_pair_to_codepoint([s characterAtIndex:0],[s characterAtIndex:1]);
        }
        else
        {
            result = [s characterAtIndex:0];
        }
        return result;
    }
}

char *bb_upper(char *string)
{
    @autoreleasepool
    {
        NSString *s,*u;
        char *utf8_string,*result;
        s = [[NSString alloc] initWithUTF8String:string];
        u = [s localizedUppercaseString];
        utf8_string = (char*)[u UTF8String];
        result = malloc(strlen(utf8_string) + 1);
        strcpy(result,utf8_string);
        return result;

    }
}

char *bb_lower(char *string)
{
    @autoreleasepool
    {
        NSString *s,*u;
        char *utf8_string,*result;
        s = [[NSString alloc] initWithUTF8String:string];
        u = [s localizedLowercaseString];
        utf8_string = (char*)[u UTF8String];
        result = malloc(strlen(utf8_string) + 1);
        strcpy(result,utf8_string);
        return result;

    }
}

#elif WINDOWS==1

long long int bb_len(char *string);
unsigned long long int surrogate_pair_to_codepoint(WCHAR lead_surrogate,WCHAR trail_surrogate);
int is_lead_surrogate(WCHAR character);
int is_trail_surrogate(WCHAR character);

long long int utf16_strlen(LPWSTR string)
{
    WCHAR current_char;
    long long int i = 0;
    long long int count = 0;
    while(1)
    {
        current_char = string[i];
        if(current_char == 0)
            break;
        if(is_lead_surrogate(current_char))
        {
            count += 1;
            i += 2;
        }
        else
        {
            count += 1;
            i = i + 1;
        }
    }
    return count;
}

long long int utf16_skip_chars(LPWSTR string,long long int num_chars)
{
    WCHAR current_char;
    long long int i = 0;
    long long int count = 0;
    while(1)
    {
        current_char = string[count];
        if(current_char == 0 || i == num_chars)
            break;
        if(is_lead_surrogate(current_char))
        {
            count += 2;
            i = i + 1;
        }
        else
        {
            count += 1;
            i = i + 1;
        }
    }
    return count;
}

long long int bb_len(char *string)
{
    long long int length;
    LPWSTR input_buffer;
    unsigned long long int result;
    int i;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);
    input_buffer = malloc((length + 1) * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
    input_buffer[length] = 0;
    result = utf16_strlen(input_buffer);
    free(input_buffer);
    return result;
}

char *bb_mid(char *string,long long int offset,long long int num_characters)
{
    long long int length;
    long long int len;
    long long int character_offset;
    LPWSTR input_buffer;
    char *output_buffer;
    int size;
    long long int prefix;
    long long int extent;

    offset--;
    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,0,0);
    len = bb_len(string);
    if(num_characters <= 0 || len == 0)
        return "";
    if(offset + num_characters > len)
    {
        if(offset == len)
        {
            return "";
        }
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        prefix = utf16_skip_chars(input_buffer,offset);
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
    else
    {
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        prefix = utf16_skip_chars(input_buffer,offset);
        extent = utf16_skip_chars(input_buffer + prefix,num_characters);
        input_buffer[prefix + extent] = 0;
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
}

char *bb_left(char *string,long long int num_characters)
{
    long long int length;
    long long int len;
    LPWSTR input_buffer;
    char *output_buffer;
    int size;
    long long int extent;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);
    len = bb_len(string);
    if(len == 0)
        return "";
    if(num_characters <= 0)
        return "";
    if(len <= num_characters)
    {
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
    else
    {
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        extent = utf16_skip_chars(input_buffer,num_characters);
        input_buffer[extent] = 0;
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
}

char *bb_right(char *string,long long int num_characters)
{
    long long int length;
    long long int len;
    LPWSTR input_buffer;
    char *output_buffer;
    int size;
    long long int prefix;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);
    len = bb_len(string);
    if(len == 0)
        return "";
    if(num_characters <= 0)
        return "";
    if(len <= num_characters)
    {
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
    else
    {
        input_buffer = malloc(length * sizeof(WCHAR));
        MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
        prefix = utf16_skip_chars(input_buffer,len - num_characters);
        size = WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,(LPSTR)0,0,0,0);
        output_buffer = malloc(size);
        WideCharToMultiByte(CP_UTF8,0,input_buffer + prefix,-1,output_buffer,size,0,0);
        free(input_buffer);
        return (char*)output_buffer;
    }
}

unsigned long long int bb_uni(char *string)
{
    long long int length;
    LPWSTR input_buffer;
    unsigned long long int result;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);
    input_buffer = malloc(length * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
    if (is_lead_surrogate(input_buffer[0]))
    {
        if(length == 1 || (!is_trail_surrogate(input_buffer[1])))
        {
            free(input_buffer);
            return 0;
        }
        result = surrogate_pair_to_codepoint(input_buffer[0],input_buffer[1]);
    }
    else
    {
        result = input_buffer[0];
    }
    free(input_buffer);
    return result;
}

unsigned long long int surrogate_pair_to_codepoint(WCHAR lead_surrogate,WCHAR trail_surrogate)
{
    unsigned short unbiased_lead = lead_surrogate - 0xD800;
    unsigned short unbiased_trail = trail_surrogate - 0xDC00;
    unsigned long long int result = ((unbiased_lead << 10) | unbiased_trail) + 0x010000;
    return result;
}

void codepoint_to_surrogate_pair(unsigned long long int codepoint,WCHAR *lead_surrogate,WCHAR *trail_surrogate)
{
    codepoint -= 0x10000;
    *lead_surrogate = ((codepoint >> 10) & 0x3FF) + 0xD800;
    *trail_surrogate = (codepoint & 0x3FF) + 0xDC00;
    return;
}

int is_lead_surrogate(WCHAR character)
{
    int result = character >= 0xD800 && character <= 0xDBFF;
    return result;
}

int is_trail_surrogate(WCHAR character)
{
    int result = character >= 0xDC00 && character <= 0xDFFF;
    return result;
}

char *bb_chr(unsigned long long int value)
{
    WCHAR lead_surrogate;
    WCHAR trail_surrogate;
    WCHAR utf16_string[3];
    char *output_buffer;
    int size;

    if(value > 0xFFFF)
    {
        codepoint_to_surrogate_pair(value,&lead_surrogate,&trail_surrogate);
        utf16_string[0] = lead_surrogate;
        utf16_string[1] = trail_surrogate;
        utf16_string[2] = 0;
    }
    else
    {
        utf16_string[0] = value;
        utf16_string[1] = 0;
    }
    size = WideCharToMultiByte(CP_UTF8,0,utf16_string,-1,(LPSTR)0,0,0,0);
    output_buffer = malloc(size);
    WideCharToMultiByte(CP_UTF8,0,utf16_string,-1,output_buffer,size,0,0);
    return (char*)output_buffer;
}

char *bb_upper(char *string)
{
    long long int length;
    LPWSTR input_buffer;
    char *output_buffer;
    int size;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);

    input_buffer = malloc(length * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
    CharUpper(input_buffer);
    size = WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,(LPSTR)0,0,0,0);
    output_buffer = malloc(size);
    WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,output_buffer,size,0,0);
    free(input_buffer);
    return (char*)output_buffer;
}

char *bb_lower(char *string)
{
    long long int length;
    LPWSTR input_buffer;
    char *output_buffer;
    int size;

    length = MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,NULL,0);

    input_buffer = malloc(length * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8,0,(LPCSTR)string,-1,input_buffer,length);
    CharLower(input_buffer);
    size = WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,(LPSTR)0,0,0,0);
    output_buffer = malloc(size);
    WideCharToMultiByte(CP_UTF8,0,input_buffer,-1,output_buffer,size,0,0);
    free(input_buffer);
    return (char*)output_buffer;
}

#endif

char *bb_lset(char *string,long long int size)
{
    char *padded_string;
    int i;
    if(bb_len(string) >= size)
        return bb_duplicate_string(string);

    padded_string = malloc(size + 1);
    strcpy(padded_string,string);
    for(i = bb_len(string); i < size; i++)
        padded_string[i] = ' ';
    padded_string[size] = '\0';
    return padded_string;
}

char *bb_rset(char *string,long long int size)
{
    char *padded_string;
    int i;
    if(bb_len(string) >= size)
        return bb_duplicate_string(string);

    padded_string = malloc(size + 1);
    strcpy(padded_string + (size - bb_len(string)),string);
    for(i = 0; i < size - bb_len(string); i++)
        padded_string[i] = ' ';
    return padded_string;
}

long long int bb_asc(char *string)
{
    return (long long int)(*string);
}

char *bb_str(double float_input)
{
    char *string;
    BB_TEMP_STRING_BUFFER[0] = 0;
    sprintf(BB_TEMP_STRING_BUFFER,"%.15f",float_input);
    string = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
    strcpy(string,BB_TEMP_STRING_BUFFER);
    return string;
}

char *bb_spc(long long int num_spaces)
{
    char *string = malloc(num_spaces + 1);
    memset(string,' ',num_spaces);
    string[num_spaces] = '\0';
    return string;
}

long long int bb_convert_string_to_int(char *string_input)
{
    return strtol(string_input,0,10);
}

double bb_convert_string_to_float(char *string_input)
{
    return strtof(string_input,0);
}

double bb_val(char *string_input)
{
    return strtof(string_input,0);
}

unsigned long long int bb_print(char *text)
{
    fprintf(stdout,"%s%s",text,NEW_LINE);
    fflush(stdout);
    return 0;
}

void bb_write(char *string_input)
{
    fprintf(stdout,"%s",string_input);
    fflush(stdout);
}

char *bb_input(char *prompt)
{
    char *result;
    puts(prompt);
    fflush(stdout);
    BB_TEMP_STRING_BUFFER[0] = 0;
    result = fgets(BB_TEMP_STRING_BUFFER,BB_INPUT_MAX - 1,stdin);
    BB_TEMP_STRING_BUFFER[strlen(BB_TEMP_STRING_BUFFER) - 1] = 0;
    return bb_duplicate_string(BB_TEMP_STRING_BUFFER);
}

void bb_restore(void *label)
{
    BB_DATA_POINTER = label;
}

void bb_read(void *dest,long long int dest_type)
{
    long long int source_type;

    if(BB_DATA_POINTER == 0)
        bb_fatal_error("'Read' without 'Restore'.");
    source_type = *BB_DATA_POINTER;
    if(source_type == BB_OUT_OF_DATA)
        bb_fatal_error("Out of data.");

    BB_DATA_POINTER++;

    switch(source_type)
    {
    case BB_DATA_TYPE_INT:
    {
        switch(dest_type)
        {
        case BB_DATA_TYPE_INT:
        {
            *((long long int*)dest) = *BB_DATA_POINTER;
            break;
        }
        case BB_DATA_TYPE_FLOAT:
        {
            *((double*)dest) = *BB_DATA_POINTER;
            break;
        }
        case BB_DATA_TYPE_STRING:
        {
            sprintf(BB_TEMP_STRING_BUFFER,"%lld",*BB_DATA_POINTER);
            *((char**)dest) = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
            strcpy(*((char**)dest),BB_TEMP_STRING_BUFFER);
            break;
        }
        }
        break;
    }
    case BB_DATA_TYPE_FLOAT:
    {
        switch(dest_type)
        {
        case BB_DATA_TYPE_INT:
        {
            *((long long int*)dest) = *(double*)BB_DATA_POINTER;
            break;
        }
        case BB_DATA_TYPE_FLOAT:
        {
            *((double*)dest) = *((double*)(BB_DATA_POINTER));
            break;
        }
        case BB_DATA_TYPE_STRING:
        {
            sprintf(BB_TEMP_STRING_BUFFER,"%.15f",*(double*)BB_DATA_POINTER);
            *((char**)dest) = malloc(strlen(BB_TEMP_STRING_BUFFER) + 1);
            strcpy(*((char**)dest),BB_TEMP_STRING_BUFFER);
            break;
        }
        }
        break;
    }
    case BB_DATA_TYPE_STRING:
    {
        switch(dest_type)
        {
        case BB_DATA_TYPE_INT:
        {
            *((long long int*)dest) = strtol(*((char**)BB_DATA_POINTER),0,10);
            break;
        }
        case BB_DATA_TYPE_FLOAT:
        {
            *((double*)dest) = strtof(*((char**)BB_DATA_POINTER),0);
            break;
        }
        case BB_DATA_TYPE_STRING:
        {
            char *source;
            source = *((char**)(BB_DATA_POINTER));
            *((char**)dest) = malloc(strlen(source) + 1);
            strcpy(*((char**)dest),source);
        }
        }
        break;
    }
    }


    BB_DATA_POINTER++;
}

double bb_abs(double arg)
{
    return fabs(arg);
}

double bb_pow(double base, double exponent)
{
    return pow(base,exponent);
}

double bb_fmod(double x, double y)
{
    return fmod(x,y);
}

void *bb_allocate_array(unsigned long long int **array,void(*init_func)(void*),void(*final_func)(void*),unsigned long long int cell_size,unsigned long long int dimensionality,...)
{
    unsigned long long int dimensions[BB_ARRAY_MAX_DIMENSIONALITY];
    unsigned long long int num_cells = 1;
    unsigned long long int size;
    unsigned long long int dim_sum = 0;
    va_list variable_arg_list;
    long long int i;

    if(*array)
    {
        bb_deallocate_array(*array,final_func,dimensionality);
        *array = 0;
    }

    va_start(variable_arg_list,dimensionality);
    for(i = 0; i < dimensionality; i++)
    {
        dimensions[i] = va_arg(variable_arg_list,unsigned long long int);
        dim_sum += dimensions[i] + 1;
        num_cells *= dimensions[i] + 1;
    }

    *array = malloc(cell_size * num_cells + (dimensionality + 1) * sizeof(unsigned long long int));

    if(!(*array)) bb_fatal_error("Array allocation failed (out of memory).");

    (*array) += 1 + dimensionality;

    if(init_func)
    {
        for(i = 0; i < num_cells; i++)
            (*init_func)(&(*array)[i]);
    }

    (*array)[-1] = cell_size;

    for(i = 0; i < dimensionality; i++)
        (*array)[(-i)-2] = dimensions[i] + 1;

    va_end(variable_arg_list);
    return (void*)0;
}

void *bb_access_array(unsigned long long int *array,unsigned long long int dimensionality,...)
{
    va_list variable_arg_list;
    unsigned long long int cell_size;
    unsigned long long int offset = 0;
    unsigned long long int accumulator = 1;
    unsigned long long int dimension;
    unsigned long long int subscript;
    int i;

    cell_size = array[-1];

    va_start(variable_arg_list,dimensionality);
    for(i = 0; i < dimensionality; i++)
    {
        subscript = va_arg(variable_arg_list,unsigned long long int);
        dimension = array[(-i)-2];
        if(subscript >= dimension)  bb_fatal_error("Array subscript out of bounds.");
        offset += accumulator * subscript;
        accumulator *= dimension;
    }

    va_end(variable_arg_list);

    return ((char*)array) + offset * cell_size;

}

void bb_deallocate_array(unsigned long long int *array,void(*final_func)(void*),unsigned long long int dimensionality)
{
    int i;
    long long int num_cells = 1;

    if(array)
    {
        if(final_func)
        {
            for(i = 0; i < dimensionality; i++)
                num_cells *= array[(-i)-2];

            for(i = 0; i < num_cells; i++)
                (*final_func)((void*)array[i]);
        }

        free(array - 1 - dimensionality);
    }
}

LinkedList *bb_create_linked_list(unsigned long long int element_size,unsigned long long int *string_offsets)
{
    LinkedList *list;

    list = malloc(sizeof(LinkedList));
    list->first = NULL;
    list->last = NULL;
    list->cached = NULL;
    list->element_size = element_size;
    list->string_offsets = string_offsets;
    list->for_each_index = 0;
    memset(list->for_each_stack,0,sizeof(void*) * BB_MAX_FOR_EACH_STACK);
    return list;
}

void bb_destroy_linked_list(LinkedList *list)
{
    bb_delete_each(list);
    free(list);
}

unsigned long long int bb_new(LinkedList *list)
{
    unsigned long long int *element;

    element = malloc(sizeof(unsigned long long int) * (list->element_size + 1));
    memset(element,0,sizeof(unsigned long long int) * (list->element_size + 1));
    list->first = g_list_prepend(list->first,(gpointer)(element + 1));
    *(GList**)element = list->first;
    if(list->last == NULL)
        list->last = list->first;
    return (unsigned long long int)list->first->data;
}

unsigned long long int bb_first(LinkedList *list)
{
    if(list->last)
        return (unsigned long long int)(list->last->data);
    else
        return 0;
}

unsigned long long int bb_last(LinkedList *list)
{
    if(list->first)
        return (unsigned long long int)(list->first->data);
    else
        return 0;
}

unsigned long long int bb_before(unsigned long long int *element)
{
    GList *current;
    GList *previous;
    current = (GList*)(*(element - 1));
    previous = g_list_next(current);
    if(previous != NULL)
        return (unsigned long long int)previous->data;
    else
        return 0;
}

unsigned long long int bb_after(unsigned long long int *element)
{
    GList *current;
    GList *next;
    current = (GList*)(*(element - 1));
    next = g_list_previous(current);
    if(next != NULL)
        return (unsigned long long int)next->data;
    else
        return 0;
}

void bb_insert_before(LinkedList *list,unsigned long long int *left_element,unsigned long long int *right_element)
{
    GList *left_list;
    GList *right_list;

    left_list = (GList*)(*(left_element - 1));
    right_list = (GList*)(*(right_element - 1));

    if(list->for_each_index > 0)
        (*(list->for_each_stack[list->for_each_index - 1])) = left_list->data;

    if(left_element == right_element)
        return;

    if(left_list == list->last) //
        list->last = g_list_previous(left_list); //

    list->first = g_list_remove_link(list->first,left_list);
    list->first = g_list_insert_before(list->first,g_list_next(right_list),left_list->data);
    *(((GList**)left_list->data) - 1) = g_list_next(right_list);
    if(right_list == list->last)
        list->last = g_list_next(right_list);
    g_list_free_1(left_list);
}

void bb_insert_after(LinkedList *list,unsigned long long int *left_element,unsigned long long int *right_element)
{
    GList *left_list;
    GList *right_list;

    left_list = (GList*)(*(left_element - 1));
    right_list = (GList*)(*(right_element - 1));

    if(list->for_each_index > 0)
        (*(list->for_each_stack[list->for_each_index - 1])) = left_list->data;

    if(left_element == right_element)
        return;

    if(left_list == list->last)
        list->last = g_list_previous(list->last);

    list->first = g_list_remove_link(list->first,left_list);
    list->first = g_list_insert_before(list->first,right_list,left_list->data);
    *(((GList**)left_list->data) - 1) = g_list_previous(right_list);
    g_list_free_1(left_list);
}

unsigned long long int bb_init_for_each(LinkedList *list,unsigned long long int **element)
{
    if(list->for_each_index == BB_MAX_FOR_EACH_STACK - 1)
        bb_fatal_error("Stack too large.");

    if(list->last == NULL)
        return 0;

    *element = list->last->data;
    list->for_each_stack[list->for_each_index] = element;
    list->for_each_index++;

    return 1;
}

unsigned long long int bb_next_for_each(LinkedList *list)
{
    GList *current;
    GList *next;

    if(list->for_each_stack[list->for_each_index - 1] == 0)
    {
        return 0;
    }

    if(list->cached)
    {
        (*(list->for_each_stack[list->for_each_index - 1])) = list->cached->data;
        list->cached = NULL;
        if(!(*(list->for_each_stack[list->for_each_index - 1])))
        {
            return 0;
        }

        current = (GList*) (*((*(list->for_each_stack[list->for_each_index - 1])) - 1));

        if(current == NULL)
        {
            return 0;
        }

        (*(list->for_each_stack[list->for_each_index - 1])) = current->data;
    }
    else
    {
        if(!(*(list->for_each_stack[list->for_each_index - 1])))
        {
            return 0;
        }

        current = (GList*) (*((*(list->for_each_stack[list->for_each_index - 1])) - 1));
        next = g_list_previous(current);

        if(next == NULL)
        {
            return 0;
        }

        (*(list->for_each_stack[list->for_each_index - 1])) = next->data;
    }

    return 1;
}

void bb_final_for_each(LinkedList *list)
{
    list->for_each_stack[list->for_each_index - 1] = 0;
    list->for_each_index--;
}

void bb_delete(LinkedList *list,unsigned long long int *element)
{
    GList *current;
    GList *previous;
    GList *next;
    GList *iterator;

    if(element == 0)
        return;

    current = (GList*)(*(element - 1));
    previous = g_list_next(current);
    next = g_list_previous(current);

    if(current == list->last && current == list->first)
    {
        if(list->for_each_index > 0 && list->for_each_stack[list->for_each_index - 1])
            (*(list->for_each_stack[list->for_each_index - 1])) = 0;
        list->first = g_list_remove_link(list->first,current);
        bb_free_strings((unsigned long long int**)current->data,list->string_offsets);
        free(((unsigned long long int*)current->data) - 1);
        g_list_free_1(current);
        list->first = list->last = list->cached = NULL;

        return;
    }
    else
    {
        if(list->for_each_index > 0 && list->for_each_stack[list->for_each_index - 1])
        {
            if(!(*(list->for_each_stack[list->for_each_index - 1])))
                list->cached = next;
            else
            {
                iterator = (GList*)(*((*(list->for_each_stack[list->for_each_index - 1])) - 1));
                if(iterator == current)
                {
                    list->cached = next;
                    (*(list->for_each_stack[list->for_each_index - 1])) = 0;
                }
            }
        }

        list->first = g_list_remove_link(list->first,current);

        if(current == list->last)
        {
            list->last = next;
        }
        else if(current == list->first)
        {
            list->first = previous;
        }
        bb_free_strings((unsigned long long int**)current->data,list->string_offsets);
        free(((unsigned long long int*)current->data) - 1);
        g_list_free_1(current);
    }
}

void bb_delete_each(LinkedList *list)
{
    GList *iterator = list->first;
    GList *next = NULL;

    while(iterator)
    {
        next = g_list_next(iterator);
        bb_delete(list,iterator->data);
        iterator = next;
    }
    list->first = NULL;
    list->last = NULL;
}

void(*bb_get_free_string())(char*)
{
    return bb_free_string;
}

void(*bb_get_free_strings())(unsigned long long int **address,unsigned long long int *offsets)
{
    return bb_free_strings;
}

void bb_free_strings(unsigned long long int **address,unsigned long long int *offsets)
{
    unsigned long long int offset;
    for(;;)
    {
        offset = *offsets;
        if(offset == BB_OUT_OF_DATA)
            break;
        bb_free_string((char*)(*(address + offset)));
        offsets++;
    }
}

void bb_debuglog(char *msg)
{
    fprintf(stderr,"%s\n",msg);
    fflush(stderr);
}
