#include "topLevelConsts_sics.h"
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


static uint32_t const v2[5] __attribute__((aligned(256))) = {2,
                                                             3,
                                                             4,
                                                             5,
                                                             6};

static uint32_t const v3[5] __attribute__((aligned(256))) = {1,
                                                             2,
                                                             3,
                                                             4,
                                                             5};

void topLevelConsts__sics(uint32_t v0, uint32_t v1, uint32_t * out)
{
  uint32_t v4;
  uint32_t v5;
  
  v4 = (v0 < 5);
  v5 = (v1 + 5);
  if (v4)
  {
    *out = v2[v5];
  }
  else
  {
    *out = v3[v5];
  }
}
