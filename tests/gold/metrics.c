#include "metrics.h"


void metrics(struct array * v0, struct array * v1, struct array * v2, struct array * * out)
{
  uint32_t v37;
  uint32_t v30;
  struct array * e40 = NULL;
  uint32_t v31;
  struct array * v34 = NULL;
  struct s_2_arr_signedS32_arr_signedS32 seq41 = { .member1 = NULL, .member2 = NULL };
  struct array * v38 = NULL;
  uint32_t v32;
  struct array * v39 = NULL;
  uint32_t v35;
  
  v37 = getLength(v2);
  e40 = initArray(e40, sizeof(uint32_t), 1);
  at(uint32_t,e40,0) = v37;
  v30 = at(uint32_t,e40,0);
  v31 = getLength(v0);
  v34 = initArray(v34, (0 - sizeof(struct array *)), v37);
  (seq41).member2 = initArray((seq41).member2, sizeof(int32_t), 8);
  for (uint32_t v4 = 0; v4 < 8; v4 += 1)
  {
    at(int32_t,(seq41).member2,v4) = -32678;
  }
  for (uint32_t v5 = 0; v5 < v37; v5 += 1)
  {
    v38 = at(struct array *,v2,v5);
    v32 = min(getLength(v38), v31);
    (seq41).member1 = initArray((seq41).member1, sizeof(int32_t), v32);
    for (uint32_t v7 = 0; v7 < v32; v7 += 1)
    {
      at(int32_t,(seq41).member1,v7) = at(int32_t,(seq41).member2,(at(struct s_2_unsignedS32_unsignedS32,v38,v7)).member1);
    }
    (seq41).member2 = initArray((seq41).member2, sizeof(int32_t), v32);
    for (uint32_t v8 = 0; v8 < v32; v8 += 1)
    {
      at(int32_t,(seq41).member2,v8) = at(int32_t,(seq41).member2,(at(struct s_2_unsignedS32_unsignedS32,v38,v8)).member1);
    }
    at(struct array *,v34,v5) = initArray(at(struct array *,v34,v5), sizeof(int32_t), getLength((seq41).member1));
    copyArray(at(struct array *,v34,v5), (seq41).member1);
  }
  *out = initArray(*out, (0 - sizeof(struct array *)), v30);
  for (uint32_t v3 = 0; v3 < v30; v3 += 1)
  {
    v39 = at(struct array *,v34,v3);
    v35 = getLength(v39);
    at(struct array *,*out,v3) = initArray(at(struct array *,*out,v3), sizeof(int32_t), v35);
    for (uint32_t v14 = 0; v14 < v35; v14 += 1)
    {
      at(int32_t,at(struct array *,*out,v3),v14) = at(int32_t,v39,v14);
    }
  }
  freeArray(e40);
  freeArray(v34);
  freeArray(v38);
  freeArray(v39);
}
