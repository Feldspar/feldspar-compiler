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
  struct array x0 = {0};
  struct array x1 = {0};
  
  v2 = (v1 + 5);
  if ((v0 < 5))
  {
    initArray(&x0, sizeof(uint32_t), 5);
    at(uint32_t,&x0,0) = 2;
    at(uint32_t,&x0,1) = 3;
    at(uint32_t,&x0,2) = 4;
    at(uint32_t,&x0,3) = 5;
    at(uint32_t,&x0,4) = 6;
    *out = at(uint32_t,&x0,v2);
  }
  else
  {
    initArray(&x1, sizeof(uint32_t), 5);
    at(uint32_t,&x1,0) = 1;
    at(uint32_t,&x1,1) = 2;
    at(uint32_t,&x1,2) = 3;
    at(uint32_t,&x1,3) = 4;
    at(uint32_t,&x1,4) = 5;
    *out = at(uint32_t,&x1,v2);
  }
  freeArray(&x0);
  freeArray(&x1);
}
