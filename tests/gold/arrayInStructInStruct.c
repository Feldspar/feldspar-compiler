#include "arrayInStructInStruct.h"
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


void arrayInStructInStruct(struct s_unsignedS32_s_unsignedS32_arr_unsignedS32_UD * v0, struct s_unsignedS32_s_unsignedS32_arr_unsignedS32_UD * out)
{
  (*out).member1 = (*v0).member1;
  ((*out).member2).member1 = ((*v0).member2).member1;
  ((*out).member2).member2 = initArray(((*out).member2).member2, sizeof(uint32_t), getLength(((*v0).member2).member2));
  copyArray(((*out).member2).member2, ((*v0).member2).member2);
}
