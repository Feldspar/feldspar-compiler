#include "concatV_build_test.h"
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


void concatV(struct array * v0, struct array * * out)
{
  uint32_t len0;
  struct array * v7 = NULL;
  
  len0 = getLength(v0);
  *out = initArray(*out, sizeof(int32_t), 0);
  for (uint32_t v6 = 0; v6 < len0; v6 += 1)
  {
    v7 = initArray(v7, sizeof(int32_t), (getLength(*out) + getLength(at(struct array *,v0,v6))));
    copyArray(v7, *out);
    copyArrayPos(v7, getLength(*out), at(struct array *,v0,v6));
    *out = initArray(*out, sizeof(int32_t), getLength(v7));
    copyArray(*out, v7);
  }
  freeArray(v7);
}
