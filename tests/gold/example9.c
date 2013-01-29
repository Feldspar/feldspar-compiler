#include "example9.h"
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


void example9(int32_t *v0, int32_t *out)
{
  int32_t v1;
  
  v1 = (*v0 + 20);
  if ((*v0 < 5))
  {
    *out = (v1 * 3);
  }
  else
  {
    *out = (v1 * 30);
  }
}
