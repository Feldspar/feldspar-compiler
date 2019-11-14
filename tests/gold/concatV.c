#include "concatV.h"


void concatV(struct array * v1, struct array * * out)
{
  struct array * v32 = NULL;
  uint32_t len38;
  struct array * v7 = NULL;
  uint32_t v12;
  struct array * v9 = NULL;
  uint32_t v10;
  struct array * e39 = NULL;
  uint32_t v33;
  
  len38 = getLength(v1);
  v32 = initArray(v32, sizeof(int32_t), 0);
  for (uint32_t v6 = 0; v6 < len38; v6 += 1)
  {
    v12 = getLength(v32);
    v9 = at(struct array *,v1,v6);
    v10 = getLength(v9);
    v7 = initArray(v7, sizeof(int32_t), (v12 + v10));
    for (uint32_t v18 = 0; v18 < v12; v18 += 1)
    {
      at(int32_t,v7,v18) = at(int32_t,v32,v18);
    }
    for (uint32_t v24 = 0; v24 < v10; v24 += 1)
    {
      at(int32_t,v7,(v24 + v12)) = at(int32_t,v9,v24);
    }
    e39 = v32;
    v32 = v7;
    v7 = e39;
  }
  v33 = getLength(v32);
  *out = initArray(*out, sizeof(int32_t), v33);
  for (uint32_t v37 = 0; v37 < v33; v37 += 1)
  {
    at(int32_t,*out,v37) = at(int32_t,v32,v37);
  }
  freeArray(v32);
  freeArray(v7);
  freeArray(v9);
}
