#include "divConq3.h"


void task_core0(uint32_t v8, uint32_t v3, struct array * v1, struct array * v24)
{
  uint32_t v9;
  uint32_t v12;
  struct array * e54 = NULL;
  
  v9 = (v8 << 10);
  v12 = min(1024, (v3 - v9));
  e54 = initArray(e54, sizeof(int32_t), v12);
  for (uint32_t v15 = 0; v15 < v12; v15 += 1)
  {
    at(int32_t,e54,v15) = (at(int32_t,v1,(v15 + v9)) + 1);
  }
  ivar_put_array(at(struct ivar,v24,v8), e54);
}

void task0(void * params)
{
  run4(task_core0, uint32_t, uint32_t, struct array *, struct array *);
}

void divConq3(struct array * v1, struct array * * out)
{
  uint32_t v3;
  uint32_t v4;
  struct array * v24 = NULL;
  struct array * v49 = NULL;
  uint32_t len55;
  struct array * v28 = NULL;
  uint32_t v34;
  struct array * v31 = NULL;
  struct ivar e56;
  uint32_t v32;
  uint32_t len57;
  struct array * e58 = NULL;
  uint32_t v50;
  
  taskpool_init(4, 4, 4);
  v3 = getLength(v1);
  v4 = (v3 >> 10);
  v24 = initArray(v24, sizeof(struct ivar), v4);
  for (uint32_t v8 = 0; v8 < v4; v8 += 1)
  {
    ivar_init(&at(struct ivar,v24,v8));
    spawn4(task0, uint32_t, v8, uint32_t, v3, struct array *, v1, struct array *, v24);
  }
  len55 = getLength(v24);
  v49 = initArray(v49, sizeof(int32_t), 0);
  for (uint32_t v27 = 0; v27 < len55; v27 += 1)
  {
    v34 = getLength(v49);
    e56 = at(struct ivar,v24,v27);
    v31 = ivar_get_array_nontask(v31, e56);
    v32 = getLength(v31);
    len57 = (v34 + v32);
    v28 = initArray(v28, sizeof(int32_t), len57);
    for (uint32_t v39 = 0; v39 < v34; v39 += 1)
    {
      at(int32_t,v28,v39) = at(int32_t,v49,v39);
    }
    for (uint32_t v43 = 0; v43 < v32; v43 += 1)
    {
      at(int32_t,v28,(v43 + v34)) = at(int32_t,v31,v43);
    }
    e58 = v49;
    v49 = v28;
    v28 = e58;
  }
  v50 = getLength(v49);
  *out = initArray(*out, sizeof(int32_t), v50);
  for (uint32_t v53 = 0; v53 < v50; v53 += 1)
  {
    at(int32_t,*out,v53) = at(int32_t,v49,v53);
  }
  taskpool_shutdown();
  freeArray(v24);
  freeArray(v49);
  freeArray(v28);
  freeArray(v31);
  ivar_destroy(&e56);
}
