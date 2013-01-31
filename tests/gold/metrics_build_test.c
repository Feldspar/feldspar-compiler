#include "metrics_build_test.h"
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


void metrics(struct array * v0, struct array * v1, struct array * v2, struct array * out)
{
  uint32_t v20;
  uint32_t v21;
  struct array st0 = {0};
  struct array * v17;
  uint32_t len1;
  
  v20 = getLength(v2);
  v21 = getLength(v0);
  setLength(out, v20);
  initArray(&st0, sizeof(int32_t), 8);
  at(int32_t,&st0,0) = -32678;
  at(int32_t,&st0,1) = -32678;
  at(int32_t,&st0,2) = -32678;
  at(int32_t,&st0,3) = -32678;
  at(int32_t,&st0,4) = -32678;
  at(int32_t,&st0,5) = -32678;
  at(int32_t,&st0,6) = -32678;
  at(int32_t,&st0,7) = -32678;
  v17 = &st0;
  initArray(out, (0 - sizeof(struct array)), v20);
  for (uint32_t v16 = 0; v16 < v20; v16 += 1)
  {
    len1 = min(getLength(&at(struct array,v2,v16)), v21);
    initArray(&at(struct array,out,v16), sizeof(int32_t), len1);
    for (uint32_t v18 = 0; v18 < len1; v18 += 1)
    {
      at(int32_t,&at(struct array,out,v16),v18) = at(int32_t,v17,(at(struct s_unsignedS32_unsignedS32,&at(struct array,v2,v16),v18)).member1);
    }
    v17 = &at(struct array,out,v16);
  }
  freeArray(&st0);
}
