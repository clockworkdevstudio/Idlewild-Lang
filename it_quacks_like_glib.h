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

// Crispy, aromatic, vegan and reverse-engineered - not appropriated

#ifdef __cplusplus
extern "C" {
#endif

typedef struct GList GList;
typedef void* gpointer;
typedef void (*GDestroyNotify)(gpointer data);

struct GList
{
    void *data;
    GList *prev,*next;
};

GList *g_list_prepend(GList *list,void *data);
GList *g_list_previous(GList *list);
GList *g_list_next(GList *list);
GList *g_list_remove_link(GList *list,GList *llink);
GList *g_list_insert_before (GList *list,GList *sibling,gpointer data);
void g_list_free_1(GList *list);
void g_list_free_full(GList *list,GDestroyNotify free_func);
GList *g_list_reverse(GList *list);

#ifdef __cplusplus
}
#endif
