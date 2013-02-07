#include "scanlPush.h"
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


void scanlPush(struct array * v0, struct array * v1, struct array * * out)
{
  uint32_t v22;
  struct array * v24 = calloc(1, sizeof(struct array));
  uint32_t v23;
  struct array * v7 = calloc(1, sizeof(struct array));
  uint32_t v25;
  struct array * v15 = calloc(1, sizeof(struct array));
  uint32_t v26;
  struct array * e0 = calloc(1, sizeof(struct array));
  
  v22 = getLength(v1);
  v23 = getLength(v0);
  setLength(v24, v23);
  for (uint32_t v4 = 0; v4 < v23; v4 += 1)
  {
    at(uint32_t,v24,v4) = at(uint32_t,v0,v4);
  }
  setLength(*out, v22);
  initArray(v7, sizeof(uint32_t), getLength(v24));
  copyArray(v7, v24);
  for (uint32_t v8 = 0; v8 < v22; v8 += 1)
  {
    v25 = getLength(v7);
    setLength(v7, v25);
    for (uint32_t v11 = 0; v11 < v25; v11 += 1)
    {
    }
    initArray(v15, sizeof(uint32_t), getLength(v7));
    copyArray(v15, v7);
    v26 = getLength(v15);
    setLength(e0, v26);
    for (uint32_t v17 = 0; v17 < v26; v17 += 1)
    {
      at(uint32_t,e0,v17) = at(uint32_t,v15,v17);
    }
    initArray(at(struct array *,*out,v8), sizeof(uint32_t), getLength(e0));
    copyArray(at(struct array *,*out,v8), e0);
  }
  freeArray(v24);
  freeArray(v7);
  freeArray(v15);
  freeArray(e0);
}
