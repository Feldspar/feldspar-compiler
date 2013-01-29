#include "topLevelConsts_native.h"
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


void topLevelConsts__native(uint32_t v0, uint32_t v1, uint32_t *out)
{
  uint32_t v2;
  uint32_t x0[5];
  uint32_t x1[5];
  
  v2 = (v1 + 5);
  if ((v0 < 5))
  {
    assert(&x0);
    x0[0] = 2;
    x0[1] = 3;
    x0[2] = 4;
    x0[3] = 5;
    x0[4] = 6;
    *out = x0[v2];
  }
  else
  {
    assert(&x1);
    x1[0] = 1;
    x1[1] = 2;
    x1[2] = 3;
    x1[3] = 4;
    x1[4] = 5;
    *out = x1[v2];
  }
}
