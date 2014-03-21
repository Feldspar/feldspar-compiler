#include "metrics_build_test.h"


void metrics(struct s_arr_unsignedS32_UD_arr_signedS32_UD * v0, struct s_arr_unsignedS32_UD_arr_signedS32_UD * v1, struct s_arr_unsignedS32_UD_arr_s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD_UD * v2, struct s_arr_unsignedS32_1_arr_s_arr_unsignedS32_1_arr_signedS32_UD_UD * out)
{
  uint32_t v66;
  uint32_t v65;
  struct array * e0 = NULL;
  struct array * v71 = NULL;
  uint32_t v67;
  uint32_t len1;
  struct s_arr_unsignedS32_1_arr_signedS32_UD st2 = { .member1 = NULL, .member2 = NULL };
  struct s_arr_unsignedS32_1_arr_signedS32_UD * v15 = NULL;
  uint32_t v68;
  struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD v69 = { .member1 = NULL, .member2 = NULL };
  uint32_t v72;
  
  v65 = at(uint32_t,(*v2).member1,0);
  e0 = setLength(e0, sizeof(uint32_t), 1);
  at(uint32_t,e0,0) = v65;
  v66 = at(uint32_t,e0,0);
  v67 = at(uint32_t,(*v0).member1,0);
  len1 = at(uint32_t,(*v2).member1,0);
  (st2).member1 = initArray((st2).member1, sizeof(uint32_t), 1);
  at(uint32_t,(st2).member1,0) = 8;
  (st2).member2 = initArray((st2).member2, sizeof(int32_t), 8);
  at(int32_t,(st2).member2,0) = -32678;
  at(int32_t,(st2).member2,1) = -32678;
  at(int32_t,(st2).member2,2) = -32678;
  at(int32_t,(st2).member2,3) = -32678;
  at(int32_t,(st2).member2,4) = -32678;
  at(int32_t,(st2).member2,5) = -32678;
  at(int32_t,(st2).member2,6) = -32678;
  at(int32_t,(st2).member2,7) = -32678;
  v15 = &st2;
  v71 = initArray(v71, sizeof(struct s_arr_unsignedS32_1_arr_signedS32_UD), len1);
  for (uint32_t v14 = 0; v14 < len1; v14 += 1)
  {
    v68 = min(at(uint32_t,(at(struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD,(*v2).member2,v14)).member1,0), v67);
    (v69).member1 = initArray((v69).member1, sizeof(uint32_t), getLength((at(struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD,(*v2).member2,v14)).member1));
    copyArray((v69).member1, (at(struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD,(*v2).member2,v14)).member1);
    (v69).member2 = initArray((v69).member2, sizeof(struct s_unsignedS32_unsignedS32), getLength((at(struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD,(*v2).member2,v14)).member2));
    copyArray((v69).member2, (at(struct s_arr_unsignedS32_UD_arr_s_unsignedS32_unsignedS32_UD,(*v2).member2,v14)).member2);
    (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member1 = setLength((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member1,0) = v68;
    (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member2 = initArray((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member2, sizeof(int32_t), v68);
    for (uint32_t v18 = 0; v18 < v68; v18 += 1)
    {
      at(int32_t,(at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14)).member2,v18) = at(int32_t,(*v15).member2,(at(struct s_unsignedS32_unsignedS32,(v69).member2,v18)).member1);
    }
    v15 = &at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v14);
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v66;
  (*out).member2 = initArray((*out).member2, sizeof(struct s_arr_unsignedS32_1_arr_signedS32_UD), v66);
  for (uint32_t v9 = 0; v9 < v66; v9 += 1)
  {
    v72 = at(uint32_t,(at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v9)).member1,0);
    (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member1 = setLength((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member1,0) = v72;
    (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member2 = initArray((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member2, sizeof(int32_t), getLength((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v9)).member2));
    copyArray((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member2, (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,v71,v9)).member2);
    (at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member2 = setLength((at(struct s_arr_unsignedS32_1_arr_signedS32_UD,(*out).member2,v9)).member2, sizeof(int32_t), v72);
  }
  freeArray(e0);
  freeArray(v71);
}
