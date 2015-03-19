#include "arrayInStruct.h"


void arrayInStruct(struct array * v0, struct array * * out)
{
  struct s_2_unsignedS32_arr_unsignedS32 e4 = { .member1 = 0, .member2 = NULL };
  struct s_2_unsignedS32_arr_unsignedS32 v2 = { .member1 = 0, .member2 = NULL };
  uint32_t len5;
  struct array * e6 = NULL;
  bool v1;
  
  (e4).member1 = getLength(v0);
  (e4).member2 = initArray((e4).member2, sizeof(uint32_t), getLength(v0));
  copyArray((e4).member2, v0);
  v1 = ((e4).member1 > 0);
  while (v1)
  {
    (v2).member1 = ((e4).member1 - 1);
    len5 = getLength((e4).member2);
    (v2).member2 = initArray((v2).member2, sizeof(uint32_t), len5);
    for (uint32_t v3 = 0; v3 < len5; v3 += 1)
    {
      at(uint32_t,(v2).member2,v3) = (at(uint32_t,(e4).member2,v3) + 5);
    }
    e6 = (e4).member2;
    e4 = v2;
    (v2).member2 = e6;
    v1 = ((e4).member1 > 0);
  }
  *out = initArray(*out, sizeof(uint32_t), getLength((e4).member2));
  copyArray(*out, (e4).member2);
}
