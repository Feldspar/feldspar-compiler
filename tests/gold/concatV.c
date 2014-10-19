#include "concatV.h"


void concatV(struct s_2_arr_unsignedS32_arr_s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  struct s_2_arr_unsignedS32_arr_signedS32 v128 = { .member1 = NULL, .member2 = NULL };
  uint32_t len0;
  struct s_2_arr_unsignedS32_arr_signedS32 v65 = { .member1 = NULL, .member2 = NULL };
  uint32_t v124;
  struct s_2_arr_unsignedS32_arr_signedS32 v91 = { .member1 = NULL, .member2 = NULL };
  uint32_t v125;
  uint32_t v97;
  struct array * e1 = NULL;
  struct array * e2 = NULL;
  uint32_t v112;
  uint32_t v123;
  
  len0 = at(uint32_t,(*v0).member1,0);
  (v128).member1 = initArray((v128).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v128).member1,0) = 0;
  (v128).member2 = initArray((v128).member2, sizeof(int32_t), 0);
  for (uint32_t v64 = 0; v64 < len0; v64 += 1)
  {
    v124 = at(uint32_t,(v128).member1,0);
    (v91).member1 = initArray((v91).member1, sizeof(uint32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v64)).member1));
    copyArray((v91).member1, (at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v64)).member1);
    (v91).member2 = initArray((v91).member2, sizeof(int32_t), getLength((at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v64)).member2));
    copyArray((v91).member2, (at(struct s_2_arr_unsignedS32_arr_signedS32,(*v0).member2,v64)).member2);
    v125 = at(uint32_t,(v91).member1,0);
    v97 = (v124 + v125);
    (v65).member1 = setLength((v65).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v65).member1,0) = v97;
    (v65).member2 = setLength((v65).member2, sizeof(int32_t), v97);
    for (uint32_t v69 = 0; v69 < v124; v69 += 1)
    {
      at(int32_t,(v65).member2,v69) = at(int32_t,(v128).member2,v69);
    }
    for (uint32_t v72 = 0; v72 < v125; v72 += 1)
    {
      at(int32_t,(v65).member2,(v72 + v124)) = at(int32_t,(v91).member2,v72);
    }
    e1 = (v128).member1;
    e2 = (v128).member2;
    v128 = v65;
    (v65).member1 = e1;
    (v65).member2 = e2;
  }
  v112 = at(uint32_t,(v128).member1,0);
  v123 = at(uint32_t,(v128).member1,0);
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v123;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v123);
  for (uint32_t v46 = 0; v46 < v112; v46 += 1)
  {
    at(int32_t,(*out).member2,v46) = at(int32_t,(v128).member2,v46);
  }
}
