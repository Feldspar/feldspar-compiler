//
// Copyright (c) 2009-2011, ERICSSON AB
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, 
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the ERICSSON AB nor the names of its contributors
//       may be used to endorse or promote products derived from this software
//       without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FELDSPAR_ARRAY_H
#define FELDSPAR_ARRAY_H

#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
//#define LOG
#include "log.h"

#include <stdio.h> /* to be removed */

/* TODO qualify the names to avoid clashes with Haskell names */

struct array
{
    void*    buffer;   /* pointer to the buffer of elements */
    int32_t  length;   /* number of elements in the array */
    int32_t  elemSize; /* size of elements in bytes; (- sizeof(struct array)) for nested arrays */
    uint32_t bytes;    /* The number of bytes the buffer can hold */
};

/* Indexing into an array: */
/* Result: element of type 'type' */
#define at(type,arr,idx) (((type*)((arr)->buffer))[idx])

/* Array (re)initialization */
static inline struct array *initArray(struct array *arr, int32_t size, int32_t len)
{
    int newBytes;

    if ( !arr )
      arr = calloc(1, sizeof(struct array));

    log_3("initArray %p %d %d - enter\n", arr, size, len);
    assert(arr);
    arr->elemSize = size;
    arr->length = len;
    if( size < 0 )
        size = sizeof(struct array);
    newBytes = size * len;
    if( arr->buffer )
    {
        // Re-initialization
        log_1("initArray %p - reinitialize\n",arr);
        if( arr->bytes < newBytes )
        {
            log_3("initArray %p - realloc since %d < %d\n"
                 , arr, arr->bytes, newBytes);
            // Not enough space: reallocation needed
            arr->bytes  = newBytes;
            arr->buffer = realloc(arr->buffer, newBytes);
        }
        else
        {
            // Otherwise: space is enough, nothing to do
            log_3("initArray %p - large enough %d >= %d\n"
                 , arr, arr->bytes, newBytes);
        }
    }
    else
    {
        // First initialization
        arr->bytes = newBytes;
        arr->buffer = (void*)malloc( newBytes );        
        log_5("initArray %p - alloc %d * %d = %d bytes at %p\n"
             , arr, arr->length, arr->elemSize, newBytes, arr->buffer);
    }
    assert( arr->buffer );
    log_3("initArray %p %d %d - leave\n", arr, size, len);
    return arr;
}

// Free array
// TODO: Think about arrays escaping from their scope.
static inline void freeArray(struct array *arr)
{
    log_1("freeArray %p - enter\n", arr);
    // assert(arr);
    // if( !arr->buffer )
    // {
        // return;
    // }
    // if( arr->elemSize < 0 )
    // {
        // int i;
        // for( i=0; i<arr->length; ++i )
            // freeArray( &at(struct array,arr,i) );
    // }
    // free(arr->buffer);
    // For the sake of extra safety:
    // arr->buffer = 0;
    // arr->length = 0;
    // arr->bytes = 0;
    log_1("freeArray %p - leave\n", arr);
}

/* Deep array copy */
static inline void copyArray(struct array *to, struct array *from)
{
    assert(to);
    assert(from);
    log_2("copyArray %p %p - enter\n", to, from);
    if( from->elemSize < 0 )
    {
        log_2("copyArray %p %p - nested enter\n", to, from);
        unsigned i;
        for( i = 0; i < from->length; ++i )
        {
            struct array *to_row = &at(struct array, to, i);
            struct array *from_row = &at(struct array, from, i);
            if( !to_row->buffer )
                to_row = initArray( to_row, from_row->elemSize, from_row->length );
            copyArray( to_row, from_row );
        }
        log_2("copyArray %p %p - nested leave\n", to, from);
    }
    else
    {
        assert(to->buffer);
        assert(from->buffer);
        log_3("copyArray %p %p - memcpy %d bytes\n", to, from
            , from->length * from->elemSize);
        memcpy( to->buffer, from->buffer, from->length * from->elemSize );
    }
    log_2("copyArray %p %p - leave\n", to, from);
}

/* Deep array copy to a given position */
static inline void copyArrayPos(struct array *to, unsigned pos, struct array *from)
{
    assert(to);
    assert(from);
    log_3("copyArrayPos %p %d %p - enter\n", to, pos, from);
    if( from->elemSize < 0 )
    {
        unsigned i;
        for( i = 0; i < from->length; ++i )
            copyArray( &at(struct array, to, i + pos), &at(struct array, from, i) );
    }
    else
    {
        assert(to->buffer);
        assert(from->buffer);
        memcpy( (char*)(to->buffer) + pos * to->elemSize, from->buffer, from->length * from->elemSize );
    }
    log_3("copyArrayPos %p %d %p - leave\n", to, pos, from);
}


/* Array length */
static inline int32_t getLength(struct array *arr)
{
  assert(arr);
  return arr->length;
}

/* Reset array length */
static inline struct array *setLength(struct array *arr, int32_t size, int32_t len)
{
    if ( !arr )
      arr = calloc(1, sizeof(struct array));

    assert(arr);
    log_2("setLength %p %d - enter\n", arr, len);
    arr->elemSize = size;
    if( size < 0 )
        size = sizeof(struct array);
    int newBytes = arr->elemSize * len;
    arr->length = len;
    if( arr->bytes < newBytes )
    {
        log_3("setLength %p %d - realloc %d bytes\n", arr, len, newBytes);
        arr->buffer = realloc(arr->buffer,newBytes);
        assert(arr->buffer);
        arr->bytes  = newBytes;
    }
    log_2("setLength %p %d - leave\n", arr, len);
    return arr;
}

#endif
