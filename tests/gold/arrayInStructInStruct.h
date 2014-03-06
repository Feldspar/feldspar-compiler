#ifndef TESTS_ARRAYINSTRUCTINSTRUCT_H
#define TESTS_ARRAYINSTRUCTINSTRUCT_H

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

struct s_unsignedS32_s_unsignedS32_arr_unsignedS32_UD
{
  uint32_t member1;
  struct s_unsignedS32_arr_unsignedS32_UD member2;
};

void arrayInStructInStruct(struct s_unsignedS32_s_unsignedS32_arr_unsignedS32_UD * v0, struct s_unsignedS32_s_unsignedS32_arr_unsignedS32_UD * out);

#endif // TESTS_ARRAYINSTRUCTINSTRUCT_H
