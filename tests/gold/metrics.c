#include "metrics.h"


void metrics(struct array * v0, struct array * v1, struct array * v2, struct array * * out)
{
  uint32_t v28;
  uint32_t v29;
  struct array * st0 = NULL;
  struct array * * v16 = NULL;
  struct array * v30 = NULL;
  uint32_t len1;
  
  v28 = getLength(v2);
  v29 = getLength(v0);
  st0 = initArray(st0, sizeof(int32_t), 8);
  at(int32_t,st0,0) = -32678;
  at(int32_t,st0,1) = -32678;
  at(int32_t,st0,2) = -32678;
  at(int32_t,st0,3) = -32678;
  at(int32_t,st0,4) = -32678;
  at(int32_t,st0,5) = -32678;
  at(int32_t,st0,6) = -32678;
  at(int32_t,st0,7) = -32678;
  v16 = &st0;
  *out = initArray(*out, (0 - sizeof(struct array *)), v28);
  for (uint32_t v15 = 0; v15 < v28; v15 += 1)
  {
    v30 = at(struct array *,v2,v15);
    len1 = min(getLength(v30), v29);
    at(struct array *,*out,v15) = initArray(at(struct array *,*out,v15), sizeof(int32_t), len1);
    for (uint32_t v17 = 0; v17 < len1; v17 += 1)
    {
      at(int32_t,at(struct array *,*out,v15),v17) = at(int32_t,*v16,(at(struct s_2_unsignedS32_unsignedS32,v30,v17)).member1);
    }
    v16 = &at(struct array *,*out,v15);
  }
  *out = setLength(*out, (0 - sizeof(struct array *)), v28);
  freeArray(st0);
}
