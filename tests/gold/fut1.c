#include "fut1.h"


void task_core1(struct ivar v2)
{
  int32_t e0;
  
  ivar_get_nontask(int32_t, &e0, v2);
  ivar_put(int32_t, v2, &e0);
}

void task1(void * params)
{
  run1(task_core1, struct ivar);
}

void fut1(struct ivar v0, struct ivar * out)
{
  struct ivar v2;
  
  v2 = v0;
  for (uint32_t v1 = 0; v1 < 20; v1 += 1)
  {
    ivar_init(&v2);
    spawn1(task1, struct ivar, v2);
  }
  *out = v2;
  ivar_destroy(&v2);
}
