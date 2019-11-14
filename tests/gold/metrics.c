#include "metrics.h"


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
  v33 = initArray(v33, (0 - sizeof(struct array *)), v10);
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
  *out = initArray(*out, (0 - sizeof(struct array *)), v34);
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
  freeArray(v33);
  freeArray(v16);
  freeArray(st45);
  freeArray(v39);
}
