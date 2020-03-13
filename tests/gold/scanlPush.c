#include "scanlPush.h"


struct array * initArray_arr_unsignedS32(struct array * dst, uint32_t newLen)
{
  uint32_t oldLen;
  
  dst = allocArray(dst);
  oldLen = getLength(dst);
  if ((oldLen != newLen))
  {
    if ((oldLen < newLen))
    {
      dst = resizeArray(dst, sizeof(struct array *), newLen);
      for (int32_t i = oldLen; i < newLen; i += 1)
      {
        struct array * null_arr_0 = NULL;
        
        at(struct array *,dst,i) = null_arr_0;
      }
    }
    else
    {
      for (int32_t i = newLen; i < oldLen; i += 1)
      {
        freeArray(at(struct array *,dst,i));
      }
      dst = resizeArray(dst, sizeof(struct array *), newLen);
    }
  }
  return(dst);
}

void freeArray_arr_unsignedS32(struct array * src)
{
  for (int32_t i = 0; i < getLength(src); i += 1)
  {
    freeArray(at(struct array *,src,i));
  }
  freeArray(src);
}

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
  *out = initArray_arr_unsignedS32(*out, v9);
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
    v23 = copyArray(v23, sizeof(uint32_t), v12);
    v24 = getLength(v23);
    e27 = initArray(e27, sizeof(uint32_t), v24);
    for (uint32_t v26 = 0; v26 < v24; v26 += 1)
    {
      at(uint32_t,e27,v26) = at(uint32_t,v23,v26);
    }
    at(struct array *,*out,v13) = initArray(at(struct array *,*out,v13), sizeof(uint32_t), getLength(e27));
    at(struct array *,*out,v13) = copyArray(at(struct array *,*out,v13), sizeof(uint32_t), e27);
  }
  freeArray(v12);
  freeArray(v23);
  freeArray(e27);
}
