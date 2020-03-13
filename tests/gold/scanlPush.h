#ifndef TESTS_SCANLPUSH_H
#define TESTS_SCANLPUSH_H

#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


struct array * initArray_arr_unsignedS32(struct array * dst, uint32_t newLen);

void freeArray_arr_unsignedS32(struct array * src);

void scanlPush(struct array * v0, struct array * v1, struct array * * out);

#endif // TESTS_SCANLPUSH_H
