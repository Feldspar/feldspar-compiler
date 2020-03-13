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

/* This library contains operations on flat arrays, arrays that do not contain
   (pointers to) other arrays. For non-flat arrays, these array operations are
   implemented by code generated from the program by the ArrayOps module.
   The size argument is always the size in bytes of each element.
*/

/* TODO qualify the names to avoid clashes with Haskell names */

struct array
{
    void*    buffer;   /* pointer to the buffer of elements */
    int32_t  length;   /* number of elements in the array */
};

/// Indexing into an array:
/// Result: element of type 'type'
#define at(type,arr,idx) (((type*)((arr)->buffer))[idx])

/// Allocate and initialize a struct array if we did not have one already
static inline struct array* allocArray( struct array* src )
{
  if( src == NULL ) {
    src = malloc( sizeof(struct array) );
    src->buffer = NULL;
    src->length = 0;
  }
  return src;
}

/// Resizing an existing array (the struct array must be initialized).
static inline struct array* resizeArray( struct array* arr, int32_t size, int32_t len )
{
  assert( arr && "array not initialized" );
  arr->buffer = realloc( arr->buffer, len*size );
  arr->length = len;
  return arr;
}

/// Array (re)initialization for flat arrays.
static inline struct array* initArray( struct array* src, int32_t size, int32_t len )
{
  src = allocArray( src );
  if( len != src->length ) {
    src = resizeArray( src, size, len );
  }
  return src;
}

/// Free a flat array or an array where all the arrays it contains have been free'd already.
// TODO: Think about arrays escaping from their scope.
static inline void freeArray( struct array* arr )
{
  if( arr != NULL ) {
    free( arr->buffer );
    free( arr );
  }
}

/// Deep array copy to a given position for flat arrays.
static inline struct array* copyArrayPos( struct array* dst, int32_t size, struct array* src, int32_t pos )
{
  if( src->length > 0 ) {
    memcpy( dst->buffer + pos * size, src->buffer, src->length * size );
  }
  return dst;
}

/// Deep array copy for flat arrays.
static inline struct array* copyArray( struct array* dst, int32_t size, struct array* src )
{
  return copyArrayPos( dst, size, src, 0 );
}

/// Combined init and copy for flat arrays.
static inline struct array* initCopyArray( struct array* dst, int32_t size, struct array* src )
{
  assert(src && "source array not initialized" );
  dst = initArray( dst, size, src->length );
  return copyArrayPos( dst, size, src, 0 );
}

/// Array length
static inline int32_t getLength(struct array *arr)
{
  assert(arr && "array not initialized" );
  return arr->length;
}

#endif
