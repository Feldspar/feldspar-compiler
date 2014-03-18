#ifndef TESTS_ARRAYINSTRUCT_H
#define TESTS_ARRAYINSTRUCT_H

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


struct s_unsignedS32_arr_unsignedS32_UD
{
  uint32_t member1;
  struct array * member2;
};

void arrayInStruct(struct array * v0, struct array * * out);

#endif // TESTS_ARRAYINSTRUCT_H
