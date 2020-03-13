#include "metrics.h"


struct array * initArray_arr_signedS32(struct array * dst, uint32_t newLen)
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

void freeArray_arr_signedS32(struct array * src)
{
  for (int32_t i = 0; i < getLength(src); i += 1)
  {
    freeArray(at(struct array *,src,i));
  }
  freeArray(src);
}

void metrics(struct array * v1, struct array * v2, struct array * v3, struct array * * out)
{
  uint32_t v10;
  uint32_t v34;
  struct array * e44 = NULL;
  uint32_t v9;
  struct array * v33 = NULL;
  struct array * v16 = NULL;
  uint32_t v19;
  struct array * st45 = NULL;
  struct array * * v14 = NULL;
  struct array * v39 = NULL;
  uint32_t v40;
  
  v10 = getLength(v3);
  e44 = initArray(e44, sizeof(uint32_t), 1);
  at(uint32_t,e44,0) = v10;
  v34 = at(uint32_t,e44,0);
  v9 = getLength(v1);
  st45 = initArray(st45, sizeof(int32_t), 8);
  for (uint32_t v6 = 0; v6 < 8; v6 += 1)
  {
    at(int32_t,st45,v6) = -32678;
  }
  v14 = &st45;
  v33 = initArray_arr_signedS32(v33, v10);
  for (uint32_t v13 = 0; v13 < v10; v13 += 1)
  {
    v16 = at(struct array *,v3,v13);
    v19 = min(getLength(v16), v9);
    at(struct array *,v33,v13) = initArray(at(struct array *,v33,v13), sizeof(int32_t), v19);
    for (uint32_t v24 = 0; v24 < v19; v24 += 1)
    {
      at(int32_t,at(struct array *,v33,v13),v24) = at(int32_t,*v14,(at(struct s_2_unsignedS32_unsignedS32,v16,v24)).member1);
    }
    v14 = &at(struct array *,v33,v13);
  }
  *out = initArray_arr_signedS32(*out, v34);
  for (uint32_t v37 = 0; v37 < v34; v37 += 1)
  {
    v39 = at(struct array *,v33,v37);
    v40 = getLength(v39);
    at(struct array *,*out,v37) = initArray(at(struct array *,*out,v37), sizeof(int32_t), v40);
    for (uint32_t v43 = 0; v43 < v40; v43 += 1)
    {
      at(int32_t,at(struct array *,*out,v37),v43) = at(int32_t,v39,v43);
    }
  }
  freeArray(e44);
  freeArray_arr_signedS32(v33);
  freeArray(v16);
  freeArray(st45);
  freeArray(v39);
}
