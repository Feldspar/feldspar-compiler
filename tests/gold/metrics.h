#ifndef TESTS_METRICS_H
#define TESTS_METRICS_H

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

struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32
{
  struct array * member1;
  struct array * member2;
};

struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32
{
  struct array * member1;
  struct array * member2;
};

struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32
{
  struct array * member1;
  struct array * member2;
};

void metrics(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * v1, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32 * v2, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * out);

#endif // TESTS_METRICS_H
