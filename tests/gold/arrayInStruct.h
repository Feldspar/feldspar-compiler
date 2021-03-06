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


struct awl_unsignedS32
{
  uint32_t * buffer;
  uint32_t length;
};

struct s_2_unsignedS32_awl_unsignedS32
{
  uint32_t member1;
  struct awl_unsignedS32 member2;
};

void arrayInStruct(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out);

#endif // TESTS_ARRAYINSTRUCT_H
