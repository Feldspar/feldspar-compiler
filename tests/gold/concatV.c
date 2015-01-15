#include "concatV.h"


void concatV(struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  struct s_2_arr_unsignedS32_arr_signedS32 v103 = { .member1 = NULL, .member2 = NULL };
  uint32_t len0;
  struct s_2_arr_unsignedS32_arr_signedS32 v6 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v104 = { .member1 = NULL, .member2 = NULL };
  uint32_t v101;
  uint32_t v102;
  uint32_t v91;
  struct array * e1 = NULL;
  struct array * e2 = NULL;
  uint32_t v95;
  
  len0 = at(uint32_t,(*v0).member1,0);
  (v103).member1 = initArray((v103).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v103).member1,0) = 0;
  (v103).member2 = initArray((v103).member2, sizeof(int32_t), 0);
  for (uint32_t v5 = 0; v5 < len0; v5 += 1)
  {
    (v104).member1 = initArray((v104).member1, sizeof(uint32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v5)).member1));
    copyArray((v104).member1, (at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v5)).member1);
    (v104).member2 = initArray((v104).member2, sizeof(int32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v5)).member2));
    copyArray((v104).member2, (at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v5)).member2);
    v101 = at(uint32_t,(v103).member1,0);
    v102 = at(uint32_t,(v104).member1,0);
    v91 = (v101 + v102);
    (v6).member1 = setLength((v6).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v6).member1,0) = v91;
    (v6).member2 = setLength((v6).member2, sizeof(int32_t), v91);
    for (uint32_t v10 = 0; v10 < v101; v10 += 1)
    {
      at(int32_t,(v6).member2,v10) = at(int32_t,(v103).member2,v10);
    }
    for (uint32_t v13 = 0; v13 < v102; v13 += 1)
    {
      at(int32_t,(v6).member2,(v13 + v101)) = at(int32_t,(v104).member2,v13);
    }
    e1 = (v103).member1;
    e2 = (v103).member2;
    v103 = v6;
    (v6).member1 = e1;
    (v6).member2 = e2;
  }
  v95 = at(uint32_t,(v103).member1,0);
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v95;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v95);
  for (uint32_t v46 = 0; v46 < v95; v46 += 1)
  {
    at(int32_t,(*out).member2,v46) = at(int32_t,(v103).member2,v46);
  }
}
