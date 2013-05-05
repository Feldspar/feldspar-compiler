#include "complexWhileCond.h"
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


void complexWhileCond(int32_t v0, struct s_signedS32_signedS32 * out)
{
  int32_t v5;
  struct s_signedS32_signedS32 v2;
  uint32_t v1;
  
  (*out).member1 = 0;
  (*out).member2 = v0;
  v2 = *out;
  v5 = ((*out).member2 - (*out).member1);
  v1 = (((*out).member1 * (*out).member1) != (v5 * v5));
  while (v1)
  {
    (v2).member1 = ((*out).member1 + 1);
    (v2).member2 = (*out).member2;
    *out = v2;
    v5 = ((*out).member2 - (*out).member1);
    v1 = (((*out).member1 * (*out).member1) != (v5 * v5));
  }
}
