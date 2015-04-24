#include "ivartest.h"


void task_core0(uint32_t v0, struct ivar v1)
{
  uint32_t e2;
  
  e2 = (v0 + 1);
  ivar_put(uint32_t, v1, &e2);
}

void task0(void * params)
{
  run2(task_core0, uint32_t, struct ivar);
}

void ivartest(uint32_t v0, uint32_t * out)
{
  struct ivar v1;
  uint32_t e3;
  
  taskpool_init(4, 4, 4);
  ivar_init(&v1);
  spawn2(task0, uint32_t, v0, struct ivar, v1);
  ivar_get_nontask(uint32_t, &e3, v1);
  *out = (e3 << 1);
  taskpool_shutdown();
  ivar_destroy(&v1);
}
