#include "metrics.h"


void metrics(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * v1, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32 * v2, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v66;
  uint32_t v65;
  struct array * v71 = NULL;
  uint32_t v67;
  uint32_t len0;
  struct s_2_arr_unsignedS32_arr_signedS32 st1 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 * v15 = NULL;
  uint32_t v68;
  struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32 v69 = { .member1 = NULL, .member2 = NULL };
  uint32_t v72;
  
  v65 = at(uint32_t,(*v2).member1,0);
  v66 = v65;
  v67 = at(uint32_t,(*v0).member1,0);
  len0 = at(uint32_t,(*v2).member1,0);
  (st1).member1 = initArray((st1).member1, sizeof(uint32_t), 1);
  at(uint32_t,(st1).member1,0) = 8;
  (st1).member2 = initArray((st1).member2, sizeof(int32_t), 8);
  at(int32_t,(st1).member2,0) = -32678;
  at(int32_t,(st1).member2,1) = -32678;
  at(int32_t,(st1).member2,2) = -32678;
  at(int32_t,(st1).member2,3) = -32678;
  at(int32_t,(st1).member2,4) = -32678;
  at(int32_t,(st1).member2,5) = -32678;
  at(int32_t,(st1).member2,6) = -32678;
  at(int32_t,(st1).member2,7) = -32678;
  v15 = &st1;
  v71 = initArray(v71, sizeof(struct s_2_arr_unsignedS32_arr_signedS32), len0);
  for (uint32_t v14 = 0; v14 < len0; v14 += 1)
  {
    v68 = min(at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v14)).member1,0), v67);
    (v69).member1 = initArray((v69).member1, sizeof(uint32_t), getLength((at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v14)).member1));
    copyArray((v69).member1, (at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v14)).member1);
    (v69).member2 = initArray((v69).member2, sizeof(struct s_2_unsignedS32_unsignedS32), getLength((at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v14)).member2));
    copyArray((v69).member2, (at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v14)).member2);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member1 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member1,0) = v68;
    (at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member2 = initArray((at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member2, sizeof(int32_t), v68);
    for (uint32_t v18 = 0; v18 < v68; v18 += 1)
    {
      at(int32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14)).member2,v18) = at(int32_t,(*v15).member2,(at(struct s_2_unsignedS32_unsignedS32,(v69).member2,v18)).member1);
    }
    v15 = &at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v14);
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v66;
  (*out).member2 = initArray((*out).member2, sizeof(struct s_2_arr_unsignedS32_arr_signedS32), v66);
  for (uint32_t v9 = 0; v9 < v66; v9 += 1)
  {
    v72 = at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v9)).member1,0);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1,0) = v72;
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2 = initArray((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, sizeof(int32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v9)).member2));
    copyArray((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, (at(struct s_2_arr_unsignedS32_arr_signedS32,v71,v9)).member2);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, sizeof(int32_t), v72);
  }
  freeArray(v71);
}
