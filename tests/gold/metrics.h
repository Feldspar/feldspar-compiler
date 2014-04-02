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


struct s_arr_unsignedS32_UD_arr_signedS32_UD
{
  struct array * member1;
  struct array * member2;
};

struct s_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD
{
  struct array * member1;
  struct array * member2;
};

struct s_arr_unsignedS32_1_arr_signedS32_UD
{
  struct array * member1;
  struct array * member2;
};

struct s_arr_unsignedS32_1_arr_s_arr_unsignedS32_1_arr_signedS32_UD_UD
{
  struct array * member1;
  struct array * member2;
};

struct s_arr_unsignedS32_UD_arr_s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD_UD
{
  struct array * member1;
  struct array * member2;
};

void metrics(struct s_arr_unsignedS32_UD_arr_signedS32_UD * v0, struct s_arr_unsignedS32_UD_arr_signedS32_UD * v1, struct s_arr_unsignedS32_UD_arr_s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD_UD * v2, struct s_arr_unsignedS32_1_arr_s_arr_unsignedS32_1_arr_signedS32_UD_UD * out);

#endif // TESTS_METRICS_H
