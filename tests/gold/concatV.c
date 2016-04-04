#include "concatV.h"


void concatV(struct array * v0, struct array * * out)
{
  struct array * v64 = NULL;
  uint32_t len65;
  struct array * v3 = NULL;
  struct array * v63 = NULL;
  uint32_t v55;
  uint32_t v56;
  struct array * e66 = NULL;
  uint32_t v58;
  
  len65 = getLength(v0);
  v64 = initArray(v64, sizeof(int32_t), 0);
  for (uint32_t v2 = 0; v2 < len65; v2 += 1)
  {
    v63 = at(struct array *,v0,v2);
    v55 = getLength(v64);
    v56 = getLength(v63);
    v3 = initArray(v3, sizeof(int32_t), (v55 + v56));
    for (uint32_t v5 = 0; v5 < v55; v5 += 1)
    {
      at(int32_t,v3,v5) = at(int32_t,v64,v5);
    }
    for (uint32_t v8 = 0; v8 < v56; v8 += 1)
    {
      at(int32_t,v3,(v8 + v55)) = at(int32_t,v63,v8);
    }
    e66 = v64;
    v64 = v3;
    v3 = e66;
  }
  v58 = getLength(v64);
  *out = initArray(*out, sizeof(int32_t), v58);
  for (uint32_t v22 = 0; v22 < v58; v22 += 1)
  {
    at(int32_t,*out,v22) = at(int32_t,v64,v22);
  }
  freeArray(v64);
  freeArray(v3);
  freeArray(v63);
}
