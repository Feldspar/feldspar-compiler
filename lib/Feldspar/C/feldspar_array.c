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

#include "feldspar_array.h"
#include <string.h>
#include <assert.h>
#include <stdio.h>
//#define LOG
#include "log.h"

unsigned int feldspar_array_linker_hook = 0xDECAFBAD;

/* Deep array copy with a given length */
void copyArrayLen(struct array *to, struct array *from, int32_t len)
{
    assert(to);
    assert(from);
    log_3("copyArrayLen %p %p %d - enter\n", to, from, len);
    if( from->elemSize < 0 )
    {
        unsigned i;
        log_3("copyArrayLen %p %p %d - nested\n", to, from, len);
        for( i = 0; i < len; ++i )
            copyArray( &at(struct array, to, i), &at(struct array, from, i) );
    }
    else
    {
        assert(to->buffer);
        assert(from->buffer);
        log_4("copyArrayLen %p %p %d - memcpy %d bytes\n", to, from, len
            , len*from->elemSize);
        memcpy( to->buffer, from->buffer, len * from->elemSize );
    }
    log_3("copyArrayLen %p %p %d - leave\n", to, from, len);
}

