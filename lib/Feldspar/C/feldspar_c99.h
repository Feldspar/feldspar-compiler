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

#ifndef FELDSPAR_C99_H
#define FELDSPAR_C99_H

#include <stdint.h>
#include <complex.h>



#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#define max(X, Y)  ((X) > (Y) ? (X) : (Y))

int8_t pow_fun_int8( int8_t, int8_t );
int16_t pow_fun_int16( int16_t, int16_t );
int32_t pow_fun_int32( int32_t, int32_t );
int64_t pow_fun_int64( int64_t, int64_t );
uint8_t pow_fun_uint8( uint8_t, uint8_t );
uint16_t pow_fun_uint16( uint16_t, uint16_t );
uint32_t pow_fun_uint32( uint32_t, uint32_t );
uint64_t pow_fun_uint64( uint64_t, uint64_t );

int8_t abs_fun_int8( int8_t );
int16_t abs_fun_int16( int16_t );
int32_t abs_fun_int32( int32_t );
int64_t abs_fun_int64( int64_t );

int8_t signum_fun_int8( int8_t );
int16_t signum_fun_int16( int16_t );
int32_t signum_fun_int32( int32_t );
int64_t signum_fun_int64( int64_t );
uint8_t signum_fun_uint8( uint8_t );
uint16_t signum_fun_uint16( uint16_t );
uint32_t signum_fun_uint32( uint32_t );
uint64_t signum_fun_uint64( uint64_t );
float signum_fun_float( float );

float logBase_fun_float( float, float );



int8_t setBit_fun_int8( int8_t, uint32_t );
int16_t setBit_fun_int16( int16_t, uint32_t );
int32_t setBit_fun_int32( int32_t, uint32_t );
int64_t setBit_fun_int64( int64_t, uint32_t );
uint8_t setBit_fun_uint8( uint8_t, uint32_t );
uint16_t setBit_fun_uint16( uint16_t, uint32_t );
uint32_t setBit_fun_uint32( uint32_t, uint32_t );
uint64_t setBit_fun_uint64( uint64_t, uint32_t );

int8_t clearBit_fun_int8( int8_t, uint32_t );
int16_t clearBit_fun_int16( int16_t, uint32_t );
int32_t clearBit_fun_int32( int32_t, uint32_t );
int64_t clearBit_fun_int64( int64_t, uint32_t );
uint8_t clearBit_fun_uint8( uint8_t, uint32_t );
uint16_t clearBit_fun_uint16( uint16_t, uint32_t );
uint32_t clearBit_fun_uint32( uint32_t, uint32_t );
uint64_t clearBit_fun_uint64( uint64_t, uint32_t );

int8_t complementBit_fun_int8( int8_t, uint32_t );
int16_t complementBit_fun_int16( int16_t, uint32_t );
int32_t complementBit_fun_int32( int32_t, uint32_t );
int64_t complementBit_fun_int64( int64_t, uint32_t );
uint8_t complementBit_fun_uint8( uint8_t, uint32_t );
uint16_t complementBit_fun_uint16( uint16_t, uint32_t );
uint32_t complementBit_fun_uint32( uint32_t, uint32_t );
uint64_t complementBit_fun_uint64( uint64_t, uint32_t );

int testBit_fun_int8( int8_t, uint32_t );
int testBit_fun_int16( int16_t, uint32_t );
int testBit_fun_int32( int32_t, uint32_t );
int testBit_fun_int64( int64_t, uint32_t );
int testBit_fun_uint8( uint8_t, uint32_t );
int testBit_fun_uint16( uint16_t, uint32_t );
int testBit_fun_uint32( uint32_t, uint32_t );
int testBit_fun_uint64( uint64_t, uint32_t );

int8_t rotateL_fun_int8( int8_t, int32_t );
int16_t rotateL_fun_int16( int16_t, int32_t );
int32_t rotateL_fun_int32( int32_t, int32_t );
int64_t rotateL_fun_int64( int64_t, int32_t );
uint8_t rotateL_fun_uint8( uint8_t, int32_t );
uint16_t rotateL_fun_uint16( uint16_t, int32_t );
uint32_t rotateL_fun_uint32( uint32_t, int32_t );
uint64_t rotateL_fun_uint64( uint64_t, int32_t );

int8_t rotateR_fun_int8( int8_t, int32_t );
int16_t rotateR_fun_int16( int16_t, int32_t );
int32_t rotateR_fun_int32( int32_t, int32_t );
int64_t rotateR_fun_int64( int64_t, int32_t );
uint8_t rotateR_fun_uint8( uint8_t, int32_t );
uint16_t rotateR_fun_uint16( uint16_t, int32_t );
uint32_t rotateR_fun_uint32( uint32_t, int32_t );
uint64_t rotateR_fun_uint64( uint64_t, int32_t );

int8_t reverseBits_fun_int8( int8_t );
int16_t reverseBits_fun_int16( int16_t );
int32_t reverseBits_fun_int32( int32_t );
int64_t reverseBits_fun_int64( int64_t );
uint8_t reverseBits_fun_uint8( uint8_t );
uint16_t reverseBits_fun_uint16( uint16_t );
uint32_t reverseBits_fun_uint32( uint32_t );
uint64_t reverseBits_fun_uint64( uint64_t );

uint32_t bitScan_fun_int8( int8_t );
uint32_t bitScan_fun_int16( int16_t );
uint32_t bitScan_fun_int32( int32_t );
uint32_t bitScan_fun_int64( int64_t );
uint32_t bitScan_fun_uint8( uint8_t );
uint32_t bitScan_fun_uint16( uint16_t );
uint32_t bitScan_fun_uint32( uint32_t );
uint32_t bitScan_fun_uint64( uint64_t );

uint32_t bitCount_fun_int8( int8_t );
uint32_t bitCount_fun_int16( int16_t );
uint32_t bitCount_fun_int32( int32_t );
uint32_t bitCount_fun_int64( int64_t );
uint32_t bitCount_fun_uint8( uint8_t );
uint32_t bitCount_fun_uint16( uint16_t );
uint32_t bitCount_fun_uint32( uint32_t );
uint32_t bitCount_fun_uint64( uint64_t );



typedef struct {
    int8_t re;
    int8_t im;
} complexOf_int8;

typedef struct {
    int16_t re;
    int16_t im;
} complexOf_int16;

typedef struct {
    int32_t re;
    int32_t im;
} complexOf_int32;

typedef struct {
    int64_t re;
    int64_t im;
} complexOf_int64;

typedef struct {
    uint8_t re;
    uint8_t im;
} complexOf_uint8;

typedef struct {
    uint16_t re;
    uint16_t im;
} complexOf_uint16;

typedef struct {
    uint32_t re;
    uint32_t im;
} complexOf_uint32;

typedef struct {
    uint64_t re;
    uint64_t im;
} complexOf_uint64;

int equal_fun_complexOf_int8( complexOf_int8, complexOf_int8 );
int equal_fun_complexOf_int16( complexOf_int16, complexOf_int16 );
int equal_fun_complexOf_int32( complexOf_int32, complexOf_int32 );
int equal_fun_complexOf_int64( complexOf_int64, complexOf_int64 );
int equal_fun_complexOf_uint8( complexOf_uint8, complexOf_uint8 );
int equal_fun_complexOf_uint16( complexOf_uint16, complexOf_uint16 );
int equal_fun_complexOf_uint32( complexOf_uint32, complexOf_uint32 );
int equal_fun_complexOf_uint64( complexOf_uint64, complexOf_uint64 );

complexOf_int8 negate_fun_complexOf_int8( complexOf_int8 );
complexOf_int16 negate_fun_complexOf_int16( complexOf_int16 );
complexOf_int32 negate_fun_complexOf_int32( complexOf_int32 );
complexOf_int64 negate_fun_complexOf_int64( complexOf_int64 );
complexOf_uint8 negate_fun_complexOf_uint8( complexOf_uint8 );
complexOf_uint16 negate_fun_complexOf_uint16( complexOf_uint16 );
complexOf_uint32 negate_fun_complexOf_uint32( complexOf_uint32 );
complexOf_uint64 negate_fun_complexOf_uint64( complexOf_uint64 );

complexOf_int8 abs_fun_complexOf_int8( complexOf_int8 );
complexOf_int16 abs_fun_complexOf_int16( complexOf_int16 );
complexOf_int32 abs_fun_complexOf_int32( complexOf_int32 );
complexOf_int64 abs_fun_complexOf_int64( complexOf_int64 );
complexOf_uint8 abs_fun_complexOf_uint8( complexOf_uint8 );
complexOf_uint16 abs_fun_complexOf_uint16( complexOf_uint16 );
complexOf_uint32 abs_fun_complexOf_uint32( complexOf_uint32 );
complexOf_uint64 abs_fun_complexOf_uint64( complexOf_uint64 );

complexOf_int8 signum_fun_complexOf_int8( complexOf_int8 );
complexOf_int16 signum_fun_complexOf_int16( complexOf_int16 );
complexOf_int32 signum_fun_complexOf_int32( complexOf_int32 );
complexOf_int64 signum_fun_complexOf_int64( complexOf_int64 );
complexOf_uint8 signum_fun_complexOf_uint8( complexOf_uint8 );
complexOf_uint16 signum_fun_complexOf_uint16( complexOf_uint16 );
complexOf_uint32 signum_fun_complexOf_uint32( complexOf_uint32 );
complexOf_uint64 signum_fun_complexOf_uint64( complexOf_uint64 );
float complex signum_fun_complexOf_float( float complex );

complexOf_int8 add_fun_complexOf_int8( complexOf_int8, complexOf_int8 );
complexOf_int16 add_fun_complexOf_int16( complexOf_int16, complexOf_int16 );
complexOf_int32 add_fun_complexOf_int32( complexOf_int32, complexOf_int32 );
complexOf_int64 add_fun_complexOf_int64( complexOf_int64, complexOf_int64 );
complexOf_uint8 add_fun_complexOf_uint8( complexOf_uint8, complexOf_uint8 );
complexOf_uint16 add_fun_complexOf_uint16( complexOf_uint16, complexOf_uint16 );
complexOf_uint32 add_fun_complexOf_uint32( complexOf_uint32, complexOf_uint32 );
complexOf_uint64 add_fun_complexOf_uint64( complexOf_uint64, complexOf_uint64 );

complexOf_int8 sub_fun_complexOf_int8( complexOf_int8, complexOf_int8 );
complexOf_int16 sub_fun_complexOf_int16( complexOf_int16, complexOf_int16 );
complexOf_int32 sub_fun_complexOf_int32( complexOf_int32, complexOf_int32 );
complexOf_int64 sub_fun_complexOf_int64( complexOf_int64, complexOf_int64 );
complexOf_uint8 sub_fun_complexOf_uint8( complexOf_uint8, complexOf_uint8 );
complexOf_uint16 sub_fun_complexOf_uint16( complexOf_uint16, complexOf_uint16 );
complexOf_uint32 sub_fun_complexOf_uint32( complexOf_uint32, complexOf_uint32 );
complexOf_uint64 sub_fun_complexOf_uint64( complexOf_uint64, complexOf_uint64 );

complexOf_int8 mult_fun_complexOf_int8( complexOf_int8, complexOf_int8 );
complexOf_int16 mult_fun_complexOf_int16( complexOf_int16, complexOf_int16 );
complexOf_int32 mult_fun_complexOf_int32( complexOf_int32, complexOf_int32 );
complexOf_int64 mult_fun_complexOf_int64( complexOf_int64, complexOf_int64 );
complexOf_uint8 mult_fun_complexOf_uint8( complexOf_uint8, complexOf_uint8 );
complexOf_uint16 mult_fun_complexOf_uint16( complexOf_uint16, complexOf_uint16 );
complexOf_uint32 mult_fun_complexOf_uint32( complexOf_uint32, complexOf_uint32 );
complexOf_uint64 mult_fun_complexOf_uint64( complexOf_uint64, complexOf_uint64 );

float complex logBase_fun_complexOf_float( float complex, float complex );

complexOf_int8 complex_fun_int8( int8_t, int8_t );
complexOf_int16 complex_fun_int16( int16_t, int16_t );
complexOf_int32 complex_fun_int32( int32_t, int32_t );
complexOf_int64 complex_fun_int64( int64_t, int64_t );
complexOf_uint8 complex_fun_uint8( uint8_t, uint8_t );
complexOf_uint16 complex_fun_uint16( uint16_t, uint16_t );
complexOf_uint32 complex_fun_uint32( uint32_t, uint32_t );
complexOf_uint64 complex_fun_uint64( uint64_t, uint64_t );
float complex complex_fun_float( float, float );

int8_t creal_fun_complexOf_int8( complexOf_int8 );
int16_t creal_fun_complexOf_int16( complexOf_int16 );
int32_t creal_fun_complexOf_int32( complexOf_int32 );
int64_t creal_fun_complexOf_int64( complexOf_int64 );
uint8_t creal_fun_complexOf_uint8( complexOf_uint8 );
uint16_t creal_fun_complexOf_uint16( complexOf_uint16 );
uint32_t creal_fun_complexOf_uint32( complexOf_uint32 );
uint64_t creal_fun_complexOf_uint64( complexOf_uint64 );

int8_t cimag_fun_complexOf_int8( complexOf_int8 );
int16_t cimag_fun_complexOf_int16( complexOf_int16 );
int32_t cimag_fun_complexOf_int32( complexOf_int32 );
int64_t cimag_fun_complexOf_int64( complexOf_int64 );
uint8_t cimag_fun_complexOf_uint8( complexOf_uint8 );
uint16_t cimag_fun_complexOf_uint16( complexOf_uint16 );
uint32_t cimag_fun_complexOf_uint32( complexOf_uint32 );
uint64_t cimag_fun_complexOf_uint64( complexOf_uint64 );

complexOf_int8 conj_fun_complexOf_int8( complexOf_int8 );
complexOf_int16 conj_fun_complexOf_int16( complexOf_int16 );
complexOf_int32 conj_fun_complexOf_int32( complexOf_int32 );
complexOf_int64 conj_fun_complexOf_int64( complexOf_int64 );
complexOf_uint8 conj_fun_complexOf_uint8( complexOf_uint8 );
complexOf_uint16 conj_fun_complexOf_uint16( complexOf_uint16 );
complexOf_uint32 conj_fun_complexOf_uint32( complexOf_uint32 );
complexOf_uint64 conj_fun_complexOf_uint64( complexOf_uint64 );

int8_t magnitude_fun_complexOf_int8( complexOf_int8 );
int16_t magnitude_fun_complexOf_int16( complexOf_int16 );
int32_t magnitude_fun_complexOf_int32( complexOf_int32 );
int64_t magnitude_fun_complexOf_int64( complexOf_int64 );
uint8_t magnitude_fun_complexOf_uint8( complexOf_uint8 );
uint16_t magnitude_fun_complexOf_uint16( complexOf_uint16 );
uint32_t magnitude_fun_complexOf_uint32( complexOf_uint32 );
uint64_t magnitude_fun_complexOf_uint64( complexOf_uint64 );

int8_t phase_fun_complexOf_int8( complexOf_int8 );
int16_t phase_fun_complexOf_int16( complexOf_int16 );
int32_t phase_fun_complexOf_int32( complexOf_int32 );
int64_t phase_fun_complexOf_int64( complexOf_int64 );
uint8_t phase_fun_complexOf_uint8( complexOf_uint8 );
uint16_t phase_fun_complexOf_uint16( complexOf_uint16 );
uint32_t phase_fun_complexOf_uint32( complexOf_uint32 );
uint64_t phase_fun_complexOf_uint64( complexOf_uint64 );

complexOf_int8 mkPolar_fun_int8( int8_t, int8_t );
complexOf_int16 mkPolar_fun_int16( int16_t, int16_t );
complexOf_int32 mkPolar_fun_int32( int32_t, int32_t );
complexOf_int64 mkPolar_fun_int64( int64_t, int64_t );
complexOf_uint8 mkPolar_fun_uint8( uint8_t, uint8_t );
complexOf_uint16 mkPolar_fun_uint16( uint16_t, uint16_t );
complexOf_uint32 mkPolar_fun_uint32( uint32_t, uint32_t );
complexOf_uint64 mkPolar_fun_uint64( uint64_t, uint64_t );
float complex mkPolar_fun_float( float, float );

complexOf_int8 cis_fun_int8( int8_t );
complexOf_int16 cis_fun_int16( int16_t );
complexOf_int32 cis_fun_int32( int32_t );
complexOf_int64 cis_fun_int64( int64_t );
complexOf_uint8 cis_fun_uint8( uint8_t );
complexOf_uint16 cis_fun_uint16( uint16_t );
complexOf_uint32 cis_fun_uint32( uint32_t );
complexOf_uint64 cis_fun_uint64( uint64_t );
float complex cis_fun_float( float );



void traceStart();
void traceEnd();

void trace_int8( int8_t, int32_t );
void trace_int16( int16_t, int32_t );
void trace_int32( int32_t, int32_t );
void trace_int64( int64_t, int32_t );
void trace_uint8( uint8_t, int32_t );
void trace_uint16( uint16_t, int32_t );
void trace_uint32( uint32_t, int32_t );
void trace_uint64( uint64_t, int32_t );
void trace_float( float, int32_t );
void trace_complexOf_int8( complexOf_int8, int32_t );
void trace_complexOf_int16( complexOf_int16, int32_t );
void trace_complexOf_int32( complexOf_int32, int32_t );
void trace_complexOf_int64( complexOf_int64, int32_t );
void trace_complexOf_uint8( complexOf_uint8, int32_t );
void trace_complexOf_uint16( complexOf_uint16, int32_t );
void trace_complexOf_uint32( complexOf_uint32, int32_t );
void trace_complexOf_uint64( complexOf_uint64, int32_t );
void trace_complexOf_float( float complex, int32_t );

#endif /* FELDSPAR_C99_H */
