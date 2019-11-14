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


struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

void metrics(struct array * v1, struct array * v2, struct array * v3, struct array * * out);

#endif // TESTS_METRICS_H
