#include "scanlPush.h"


void scanlPush(struct array * v0, struct array * v1, struct array * * out)
{
  uint32_t v9;
  uint32_t v2;
  struct array * v12 = NULL;
  uint32_t v15;
  struct array * v23 = NULL;
  uint32_t v24;
  struct array * e27 = NULL;
  
  v9 = getLength(v1);
  *out = initArray(*out, (0 - sizeof(struct array *)), v9);
  v2 = getLength(v0);
  v12 = initArray(v12, sizeof(uint32_t), v2);
  for (uint32_t v4 = 0; v4 < v2; v4 += 1)
  {
    at(uint32_t,v12,v4) = at(uint32_t,v0,v4);
  }
  for (uint32_t v13 = 0; v13 < v9; v13 += 1)
  {
    v15 = getLength(v12);
    v12 = initArray(v12, sizeof(uint32_t), v15);
    v23 = initArray(v23, sizeof(uint32_t), getLength(v12));
    copyArray(v23, v12);
    v24 = getLength(v23);
    e27 = initArray(e27, sizeof(uint32_t), v24);
    for (uint32_t v26 = 0; v26 < v24; v26 += 1)
    {
      at(uint32_t,e27,v26) = at(uint32_t,v23,v26);
    }
    at(struct array *,*out,v13) = initArray(at(struct array *,*out,v13), sizeof(uint32_t), getLength(e27));
    copyArray(at(struct array *,*out,v13), e27);
  }
  freeArray(v12);
  freeArray(v23);
  freeArray(e27);
}
