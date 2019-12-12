#include "concatV.h"


void concatV(struct array * v1, struct array * * out)
{
  struct array * v26 = NULL;
  uint32_t len31;
  struct array * v6 = NULL;
  uint32_t v11;
  struct array * v8 = NULL;
  uint32_t v9;
  uint32_t len32;
  struct array * e33 = NULL;
  uint32_t v27;
  
  len31 = getLength(v1);
  v26 = initArray(v26, sizeof(int32_t), 0);
  for (uint32_t v5 = 0; v5 < len31; v5 += 1)
  {
    v11 = getLength(v26);
    v8 = at(struct array *,v1,v5);
    v9 = getLength(v8);
    len32 = (v11 + v9);
    v6 = initArray(v6, sizeof(int32_t), len32);
    for (uint32_t v16 = 0; v16 < v11; v16 += 1)
    {
      at(int32_t,v6,v16) = at(int32_t,v26,v16);
    }
    for (uint32_t v20 = 0; v20 < v9; v20 += 1)
    {
      at(int32_t,v6,(v20 + v11)) = at(int32_t,v8,v20);
    }
    e33 = v26;
    v26 = v6;
    v6 = e33;
  }
  v27 = getLength(v26);
  *out = initArray(*out, sizeof(int32_t), v27);
  for (uint32_t v30 = 0; v30 < v27; v30 += 1)
  {
    at(int32_t,*out,v30) = at(int32_t,v26,v30);
  }
  freeArray(v26);
  freeArray(v6);
  freeArray(v8);
}
