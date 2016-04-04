#include "metrics.h"


void metrics(struct array * v0, struct array * v1, struct array * v2, struct array * * out)
{
  uint32_t v28;
  uint32_t v29;
  struct array * st32 = NULL;
  struct array * * v16 = NULL;
  struct array * v30 = NULL;
  uint32_t len33;
  
  v28 = getLength(v2);
  v29 = getLength(v0);
  st32 = initArray(st32, sizeof(int32_t), 8);
  at(int32_t,st32,0) = -32678;
  at(int32_t,st32,1) = -32678;
  at(int32_t,st32,2) = -32678;
  at(int32_t,st32,3) = -32678;
  at(int32_t,st32,4) = -32678;
  at(int32_t,st32,5) = -32678;
  at(int32_t,st32,6) = -32678;
  at(int32_t,st32,7) = -32678;
  v16 = &st32;
  *out = initArray(*out, (0 - sizeof(struct array *)), v28);
  for (uint32_t v15 = 0; v15 < v28; v15 += 1)
  {
    v30 = at(struct array *,v2,v15);
    len33 = min(getLength(v30), v29);
    at(struct array *,*out,v15) = initArray(at(struct array *,*out,v15), sizeof(int32_t), len33);
    for (uint32_t v17 = 0; v17 < len33; v17 += 1)
    {
      at(int32_t,at(struct array *,*out,v15),v17) = at(int32_t,*v16,(at(struct s_2_unsignedS32_unsignedS32,v30,v17)).member1);
    }
    v16 = &at(struct array *,*out,v15);
  }
  *out = initArray(*out, (0 - sizeof(struct array *)), v28);
  freeArray(st32);
  freeArray(v30);
}
