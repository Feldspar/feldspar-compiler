#include "pairParam2.h"
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


void pairParam2(struct s_signedS16_signedS16 * v0, struct s_s_signedS16_signedS16_s_signedS16_signedS16 * out)
{
  (*out).member1 = *v0;
  (*out).member2 = *v0;
}
