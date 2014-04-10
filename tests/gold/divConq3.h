#ifndef TESTS_DIVCONQ3_H
#define TESTS_DIVCONQ3_H

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


struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32
{
  struct array * member1;
  struct array * member2;
};

struct s_2_arr_unsignedS32_arr_signedS32
{
  struct array * member1;
  struct array * member2;
};

void task_core1(struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v4, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634);

void task1(void * params);

void task_core12(struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v423, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643);

void task12(void * params);

void divConq3(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out);

#endif // TESTS_DIVCONQ3_H
