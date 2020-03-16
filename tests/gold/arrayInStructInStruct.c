#include "arrayInStructInStruct.h"


void arrayInStructInStruct(struct s_2_unsignedS32_s_2_unsignedS32_arr_unsignedS32 * v0, struct s_2_unsignedS32_s_2_unsignedS32_arr_unsignedS32 * out)
{
  (*out).member1 = (*v0).member1;
  ((*out).member2).member1 = ((*v0).member2).member1;
  ((*out).member2).member2 = initCopyArray(((*out).member2).member2, sizeof(uint32_t), ((*v0).member2).member2);
}
