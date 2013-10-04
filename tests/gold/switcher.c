#include "switcher.h"
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


void switcher(uint32_t v0, uint32_t v1, uint32_t * out)
{
  switch (v0)
  {
    0:
      *out = 1;
      break;
    1:
      *out = 2;
      break;
    default:
      *out = 0;
      break;
  }
}
