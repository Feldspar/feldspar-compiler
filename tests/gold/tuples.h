#ifndef TESTS_TUPLES_H
#define TESTS_TUPLES_H

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


struct s_2_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
};

struct s_3_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
};

struct s_4_signedS32_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
  int32_t member4;
};

struct s_5_signedS32_signedS32_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
  int32_t member4;
  int32_t member5;
};

struct s_6_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
  int32_t member4;
  int32_t member5;
  int32_t member6;
};

struct s_7_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
  int32_t member4;
  int32_t member5;
  int32_t member6;
  int32_t member7;
};

struct s_15_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
  int32_t member3;
  int32_t member4;
  int32_t member5;
  int32_t member6;
  int32_t member7;
  int32_t member8;
  int32_t member9;
  int32_t member10;
  int32_t member11;
  int32_t member12;
  int32_t member13;
  int32_t member14;
  int32_t member15;
};

void tuples(int32_t v0, int32_t * out);

#endif // TESTS_TUPLES_H
