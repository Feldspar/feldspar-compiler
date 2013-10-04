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


void switcher(uint8_t v0, uint32_t v1, uint8_t * out)
{
  switch (v1)
  {
    true:
      *out = 1;
      break;
    false:
      *out = 2;
      break;
    default:
      *out = 0;
      break;
  }
}
