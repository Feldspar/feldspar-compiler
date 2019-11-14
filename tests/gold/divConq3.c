#include "divConq3.h"


void task_core0(uint32_t v9, uint32_t v4, struct array * v1, struct array * v25)
{
  uint32_t v10;
  uint32_t v13;
  struct array * e61 = NULL;
  
  v10 = (v9 << 10);
  v13 = min(1024, (v4 - v10));
  e61 = initArray(e61, sizeof(int32_t), v13);
  for (uint32_t v16 = 0; v16 < v13; v16 += 1)
  {
    at(int32_t,e61,v16) = (at(int32_t,v1,(v16 + v10)) + 1);
  }
  ivar_put_array(at(struct ivar,v25,v9), e61);
}

void task0(void * params)
{
  run4(task_core0, uint32_t, uint32_t, struct array *, struct array *);
}

void divConq3(struct array * v1, struct array * * out)
{
  uint32_t v4;
  uint32_t v5;
  struct array * v25 = NULL;
  struct array * v55 = NULL;
  uint32_t len62;
  struct array * v29 = NULL;
  uint32_t v35;
  struct array * v32 = NULL;
  struct ivar e63;
  uint32_t v33;
  struct array * e64 = NULL;
  uint32_t v56;
  
  taskpool_init(4, 4, 4);
  v4 = getLength(v1);
  v5 = (v4 >> 10);
  v25 = initArray(v25, sizeof(struct ivar), v5);
  for (uint32_t v9 = 0; v9 < v5; v9 += 1)
  {
    ivar_init(&at(struct ivar,v25,v9));
    spawn4(task0, uint32_t, v9, uint32_t, v4, struct array *, v1, struct array *, v25);
  }
  len62 = getLength(v25);
  v55 = initArray(v55, sizeof(int32_t), 0);
  for (uint32_t v28 = 0; v28 < len62; v28 += 1)
  {
    v35 = getLength(v55);
    e63 = at(struct ivar,v25,v28);
    v32 = ivar_get_array_nontask(v32, e63);
    v33 = getLength(v32);
    v29 = initArray(v29, sizeof(int32_t), (v35 + v33));
    for (uint32_t v41 = 0; v41 < v35; v41 += 1)
    {
      at(int32_t,v29,v41) = at(int32_t,v55,v41);
    }
    for (uint32_t v47 = 0; v47 < v33; v47 += 1)
    {
      at(int32_t,v29,(v47 + v35)) = at(int32_t,v32,v47);
    }
    e64 = v55;
    v55 = v29;
    v29 = e64;
  }
  v56 = getLength(v55);
  *out = initArray(*out, sizeof(int32_t), v56);
  for (uint32_t v60 = 0; v60 < v56; v60 += 1)
  {
    at(int32_t,*out,v60) = at(int32_t,v55,v60);
  }
  taskpool_shutdown();
  freeArray(v25);
  freeArray(v55);
  freeArray(v29);
  freeArray(v32);
  ivar_destroy(&e63);
}
