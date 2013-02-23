#include "divConq3.h"
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


void task_core3(struct array * v0, uint32_t v22, uint32_t v23, struct array * v24, uint32_t v12)
{
  struct array * e1 = NULL;
  uint32_t len2;
  
  len2 = min((v22 - min(v22, v23)), 1024);
  e1 = initArray(e1, sizeof(int32_t), len2);
  for (uint32_t v13 = 0; v13 < len2; v13 += 1)
  {
    at(int32_t,e1,v13) = (at(int32_t,v0,(v13 + v23)) + 1);
  }
  ivar_put_array(at(struct ivar,v24,v12), e1);
}

void task3(void * params)
{
  run5(task_core3, struct array *, uint32_t, uint32_t, struct array *, uint32_t);
}

void divConq3(struct array * v0, struct array * * out)
{
  struct array * v24 = NULL;
  uint32_t v22;
  uint32_t len0;
  uint32_t v23;
  uint32_t len4;
  struct array * e5 = NULL;
  struct ivar e6;
  struct array * v15 = NULL;
  
  v22 = getLength(v0);
  len0 = (v22 >> 10);
  v24 = initArray(v24, sizeof(struct ivar), len0);
  for (uint32_t v12 = 0; v12 < len0; v12 += 1)
  {
    v23 = (v12 << 10);
    ivar_init(&at(struct ivar,v24,v12));
    spawn5(task3, struct array, *v0, uint32_t, v22, uint32_t, v23, struct array, *v24, uint32_t, v12);
  }
  len4 = getLength(v24);
  *out = initArray(*out, sizeof(int32_t), 0);
  for (uint32_t v14 = 0; v14 < len4; v14 += 1)
  {
    e6 = at(struct ivar,v24,v14);
    ivar_get_array_nontask(e5, e6);
    v15 = initArray(v15, sizeof(int32_t), (getLength(*out) + getLength(e5)));
    copyArray(v15, *out);
    copyArrayPos(v15, getLength(*out), e5);
    *out = initArray(*out, sizeof(int32_t), getLength(v15));
    copyArray(*out, v15);
  }
  freeArray(v24);
  freeArray(e5);
  ivar_destroy(&e6);
  freeArray(v15);
}
