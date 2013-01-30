#include "pairParam.h"
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


void pairParam(struct s_unsignedS32_unsignedS32 * v0, uint32_t * out)
{
  *out = *v0.member1;
}
