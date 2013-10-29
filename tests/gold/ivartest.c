#include "ivartest.h"
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


void task_core1(uint32_t v0, struct ivar v1)
{
  uint32_t e0;
  
  e0 = (v0 + 1);
  ivar_put(uint32_t, v1, &e0);
}

void task1(void * params)
{
  run2(task_core1, uint32_t, struct ivar);
}

void ivartest(uint32_t v0, uint32_t * out)
{
  struct ivar v1;
  uint32_t e2;
  
  ivar_init(&v1);
  spawn2(task1, uint32_t, v0, struct ivar, v1);
  ivar_get_nontask(uint32_t, &e2, v1);
  *out = (e2 << 1);
  ivar_destroy(&v1);
}
