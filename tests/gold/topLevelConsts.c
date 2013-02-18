#include "topLevelConsts.h"
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


void topLevelConsts(uint32_t v0, uint32_t v1, uint32_t * out)
{
  uint32_t v2;
  
  v2 = (v1 + 5);
  if ((v0 < 5))
  {
    *out = ((uint32_t[]){2, 3, 4, 5, 6})[v2];
  }
  else
  {
    *out = ((uint32_t[]){1, 2, 3, 4, 5})[v2];
  }
}
