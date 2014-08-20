#include "arrayInStructInStruct.h"


void arrayInStructInStruct(struct s_2_unsignedS32_s_2_unsignedS32_arr_unsignedS32 * v0, struct s_2_unsignedS32_s_2_unsignedS32_arr_unsignedS32 * out)
{
  uint32_t i0_1;
  uint32_t i0_2_1;
  struct array * i0_2_2 = NULL;
  uint32_t o1_1;
  uint32_t o1_2_1;
  struct array * o1_2_2 = NULL;
  
  i0_1 = (*v0).member1;
  i0_2_1 = ((*v0).member2).member1;
  i0_2_2 = initArray(i0_2_2, sizeof(uint32_t), getLength(((*v0).member2).member2));
  copyArray(i0_2_2, ((*v0).member2).member2);
  o1_1 = i0_1;
  o1_2_1 = i0_2_1;
  o1_2_2 = initArray(o1_2_2, sizeof(uint32_t), getLength(i0_2_2));
  copyArray(o1_2_2, i0_2_2);
  (*out).member1 = o1_1;
  ((*out).member2).member1 = o1_2_1;
  ((*out).member2).member2 = initArray(((*out).member2).member2, sizeof(uint32_t), getLength(o1_2_2));
  copyArray(((*out).member2).member2, o1_2_2);
  freeArray(i0_2_2);
  freeArray(o1_2_2);
}
