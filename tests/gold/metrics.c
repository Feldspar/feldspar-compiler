#include "metrics.h"


void metrics(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * v1, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32 * v2, struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v78;
  uint32_t v72;
  struct array * v73 = NULL;
  struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32 v65 = { .member1 = NULL, .member2 = NULL };
  uint32_t v68;
  struct s_2_arr_unsignedS32_arr_signedS32 st0 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 * v43 = NULL;
  struct s_2_arr_unsignedS32_arr_signedS32 v74 = { .member1 = NULL, .member2 = NULL };
  uint32_t v75;
  
  v78 = at(uint32_t,(*v0).member1,0);
  v72 = at(uint32_t,(*v2).member1,0);
  (st0).member1 = initArray((st0).member1, sizeof(uint32_t), 1);
  at(uint32_t,(st0).member1,0) = 8;
  (st0).member2 = initArray((st0).member2, sizeof(int32_t), 8);
  at(int32_t,(st0).member2,0) = -32678;
  at(int32_t,(st0).member2,1) = -32678;
  at(int32_t,(st0).member2,2) = -32678;
  at(int32_t,(st0).member2,3) = -32678;
  at(int32_t,(st0).member2,4) = -32678;
  at(int32_t,(st0).member2,5) = -32678;
  at(int32_t,(st0).member2,6) = -32678;
  at(int32_t,(st0).member2,7) = -32678;
  v43 = &st0;
  v73 = initArray(v73, sizeof(struct s_2_arr_unsignedS32_arr_signedS32), v72);
  for (uint32_t v42 = 0; v42 < v72; v42 += 1)
  {
    (v65).member1 = initArray((v65).member1, sizeof(uint32_t), getLength((at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v42)).member1));
    copyArray((v65).member1, (at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v42)).member1);
    (v65).member2 = initArray((v65).member2, sizeof(struct s_2_unsignedS32_unsignedS32), getLength((at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v42)).member2));
    copyArray((v65).member2, (at(struct s_2_arr_unsignedS32_arr_s_2_unsignedS32_unsignedS32,(*v2).member2,v42)).member2);
    v68 = min(at(uint32_t,(v65).member1,0), v78);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member1 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member1,0) = v68;
    (at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member2 = initArray((at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member2, sizeof(int32_t), v68);
    for (uint32_t v49 = 0; v49 < v68; v49 += 1)
    {
      at(int32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42)).member2,v49) = at(int32_t,(*v43).member2,(at(struct s_2_unsignedS32_unsignedS32,(v65).member2,v49)).member1);
    }
    v43 = &at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v42);
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v72;
  (*out).member2 = initArray((*out).member2, sizeof(struct s_2_arr_unsignedS32_arr_signedS32), v72);
  for (uint32_t v9 = 0; v9 < v72; v9 += 1)
  {
    (v74).member1 = initArray((v74).member1, sizeof(uint32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v9)).member1));
    copyArray((v74).member1, (at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v9)).member1);
    (v74).member2 = initArray((v74).member2, sizeof(int32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v9)).member2));
    copyArray((v74).member2, (at(struct s_2_arr_unsignedS32_arr_signedS32,v73,v9)).member2);
    v75 = at(uint32_t,(v74).member1,0);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1, sizeof(uint32_t), 1);
    at(uint32_t,(at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member1,0) = v75;
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2 = initArray((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, sizeof(int32_t), getLength((v74).member2));
    copyArray((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, (v74).member2);
    (at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2 = setLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*out).member2,v9)).member2, sizeof(int32_t), v75);
  }
  freeArray(v73);
}
