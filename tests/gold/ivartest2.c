#include "ivartest2.h"
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


void task_core0(struct s_unsignedS32_unsignedS32 * v0, struct ivar v3)
{
  ivar_put(struct s_unsignedS32_unsignedS32 *, v3, v0);
}

void task0(void * params)
{
  run2(task_core0, struct s_unsignedS32_unsignedS32 *, struct ivar);
}

void ivartest2(struct s_unsignedS32_unsignedS32 * v0, struct s_unsignedS32_unsignedS32 * out)
{
  struct ivar v3;
  
  ivar_init(&v3);
  spawn2(task0, struct s_unsignedS32_unsignedS32 *, v0, struct ivar, v3);
  ivar_get_nontask(struct s_unsignedS32_unsignedS32 *, out, v3);
  ivar_destroy(&v3);
}
