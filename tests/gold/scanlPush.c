#include "scanlPush.h"


void scanlPush(struct array * v0, struct array * v1, struct array * * out)
{
  uint32_t v22;
  uint32_t v23;
  struct array * v7 = NULL;
  uint32_t v25;
  struct array * v15 = NULL;
  uint32_t v26;
  struct array * e27 = NULL;
  
  v22 = getLength(v1);
  v23 = getLength(v0);
  *out = initArray(*out, (0 - sizeof(struct array *)), v22);
  v7 = initArray(v7, sizeof(uint32_t), v23);
  for (uint32_t v4 = 0; v4 < v23; v4 += 1)
  {
    at(uint32_t,v7,v4) = at(uint32_t,v0,v4);
  }
  for (uint32_t v8 = 0; v8 < v22; v8 += 1)
  {
    v25 = getLength(v7);
    v7 = initArray(v7, sizeof(uint32_t), v25);
    v15 = initArray(v15, sizeof(uint32_t), getLength(v7));
    copyArray(v15, v7);
    v26 = getLength(v15);
    e27 = initArray(e27, sizeof(uint32_t), v26);
    for (uint32_t v17 = 0; v17 < v26; v17 += 1)
    {
      at(uint32_t,e27,v17) = at(uint32_t,v15,v17);
    }
    at(struct array *,*out,v8) = initArray(at(struct array *,*out,v8), sizeof(uint32_t), getLength(e27));
    copyArray(at(struct array *,*out,v8), e27);
  }
  freeArray(v7);
  freeArray(v15);
  freeArray(e27);
}
