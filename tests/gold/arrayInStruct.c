#include "arrayInStruct.h"


void arrayInStruct(struct array * v0, struct array * * out)
{
  uint32_t e0_1;
  struct array * e0_2 = NULL;
  struct array * v2_2 = NULL;
  uint32_t len1;
  bool v1;
  
  e0_1 = getLength(v0);
  e0_2 = initArray(e0_2, sizeof(uint32_t), getLength(v0));
  copyArray(e0_2, v0);
  v1 = (e0_1 > 0);
  while (v1)
  {
    e0_1 = (e0_1 - 1);
    len1 = getLength(e0_2);
    v2_2 = initArray(v2_2, sizeof(uint32_t), len1);
    for (uint32_t v3 = 0; v3 < len1; v3 += 1)
    {
      at(uint32_t,v2_2,v3) = (at(uint32_t,e0_2,v3) + 5);
    }
    e0_1 = e0_1;
    e0_2 = v2_2;
    v1 = (e0_1 > 0);
  }
  *out = initArray(*out, sizeof(uint32_t), getLength(e0_2));
  copyArray(*out, e0_2);
  freeArray(e0_2);
  freeArray(v2_2);
}
