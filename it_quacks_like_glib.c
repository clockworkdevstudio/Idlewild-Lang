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

#include "it_quacks_like_glib.h"
#include <stdlib.h>
#include <stdio.h>

GList *g_list_prepend(GList *list,void *data)
{
    GList *new_list = malloc(sizeof(GList));
    new_list->data = data;

    if(list == NULL)
    {
        new_list->prev = NULL;
        new_list->next = NULL;
        return new_list;
    }
    else
    {
        list->prev = new_list;
        new_list->prev = NULL;
        new_list->next = list;
        return new_list;
    }
}

GList *g_list_previous(GList *list)
{
    return list->prev;
}

GList *g_list_next(GList *list)
{
    return list->next;
}

GList *g_list_remove_link(GList *list,GList *llink)
{
    GList *temp = NULL;
    if(list == llink)
        temp = list->next;
    if(llink->prev)
        llink->prev->next = llink->next;
    if(llink->next)
        llink->next->prev = llink->prev;

    llink->prev = NULL;
    llink->next = NULL;

    if(temp)
        return temp;
    else
        return list;
}

GList *g_list_insert_before (GList *list,GList *sibling,gpointer data)
{
    GList *temp = NULL;
    GList *new_list = malloc(sizeof(GList));
    new_list->data = data;

    if(sibling == NULL)
    {
        for(temp = list; temp->next; temp = temp->next);
        new_list->prev = temp;
        new_list->next = NULL;
        temp->next = new_list;
        return list;
    }
    else
    {

        if(list == sibling)
        {
            new_list->prev = NULL;
            new_list->next = sibling;
            sibling->prev = new_list;
            return new_list;
        }
        else
        {
            new_list->prev = sibling->prev;
            new_list->next = sibling;
            sibling->prev->next = new_list;
            sibling->prev = new_list;
            GList *i,*j;
            for(i = list; i; i = i->next)
            {
                if(!i->next)
                    j = i;
            }
            j = j->prev;
            j = j->prev;
            j = j->prev;
            return list;
        }
    }
}

void g_list_free_1(GList *list)
{
    free(list);
}

void g_list_free_full(GList *list,GDestroyNotify free_func)
{
    GList *holder;
    while(list)
    {
        holder = list->next;
        g_list_remove_link(list,list);
        (*free_func)(list->data);
        g_list_free_1(list);
        list = holder;
    }
}

GList *g_list_reverse(GList *list)
{
    GList *temp = NULL;
    while(list)
    {
        temp = list->prev;
        list->prev = list->next;
        list->next = temp;
        temp = list;
        list = list->prev;
    }
    return temp;
}

#ifdef __cplusplus
}
#endif
