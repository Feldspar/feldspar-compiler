#include "arrayInStruct.h"


void arrayInStruct(struct array * v0, struct array * * out)
{
  struct s_unsignedS32_arr_unsignedS32_UD e0 = { .member1 = 0, .member2 = NULL };
  struct s_unsignedS32_arr_unsignedS32_UD v2 = { .member1 = 0, .member2 = NULL };
  uint32_t len1;
  struct array * e2 = NULL;
  bool v1;
  
  (e0).member1 = getLength(v0);
  (e0).member2 = initArray((e0).member2, sizeof(uint32_t), getLength(v0));
  copyArray((e0).member2, v0);
  v1 = ((e0).member1 > 0);
  while (v1)
  {
    (v2).member1 = ((e0).member1 - 1);
    len1 = getLength((e0).member2);
    (v2).member2 = initArray((v2).member2, sizeof(uint32_t), len1);
    for (uint32_t v3 = 0; v3 < len1; v3 += 1)
    {
      at(uint32_t,(v2).member2,v3) = (at(uint32_t,(e0).member2,v3) + 5);
    }
    e2 = (e0).member2;
    e0 = v2;
    (v2).member2 = e2;
    v1 = ((e0).member1 > 0);
  }
  *out = initArray(*out, sizeof(uint32_t), getLength((e0).member2));
  copyArray(*out, (e0).member2);
}
