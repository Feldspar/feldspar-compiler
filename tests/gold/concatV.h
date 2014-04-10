#ifndef TESTS_CONCATV_H
#define TESTS_CONCATV_H

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


struct s_2_arr_unsignedS32_arr_signedS32
{
  struct array * member1;
  struct array * member2;
};

struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32
{
  struct array * member1;
  struct array * member2;
};

void concatV(struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out);

#endif // TESTS_CONCATV_H
