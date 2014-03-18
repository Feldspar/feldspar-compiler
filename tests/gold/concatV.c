#include "concatV.h"
#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


void concatV(struct s_arr_unsignedS32_UD_arr_s_arr_unsignedS32_UD_arr_signedS32_UD_UD * v0, struct s_arr_unsignedS32_1_arr_signedS32_UD * out)
{
  uint32_t v95;
  struct s_arr_unsignedS32_1_arr_signedS32_UD e0 = { .member1 = NULL, .member2 = NULL };
  uint32_t len1;
  uint32_t v91;
  uint32_t v92;
  uint32_t v93;
  struct s_arr_unsignedS32_UD_arr_signedS32_UD v94 = { .member1 = NULL, .member2 = NULL };
  struct s_arr_unsignedS32_1_arr_signedS32_UD v6 = { .member1 = NULL, .member2 = NULL };
  struct s_arr_unsignedS32_1_arr_signedS32_UD v100 = { .member1 = NULL, .member2 = NULL };
  uint32_t len2;
  uint32_t v96;
  uint32_t v97;
  uint32_t v98;
  struct s_arr_unsignedS32_UD_arr_signedS32_UD v99 = { .member1 = NULL, .member2 = NULL };
  struct s_arr_unsignedS32_1_arr_signedS32_UD v65 = { .member1 = NULL, .member2 = NULL };
  
  len1 = at(uint32_t,(*v0).member1,0);
  (v6).member1 = initArray((v6).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v6).member1,0) = 0;
  (v6).member2 = initArray((v6).member2, sizeof(int32_t), 0);
  for (uint32_t v5 = 0; v5 < len1; v5 += 1)
  {
    v91 = (at(uint32_t,(v6).member1,0) + at(uint32_t,(at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member1,0));
    v92 = at(uint32_t,(v6).member1,0);
    v93 = at(uint32_t,(at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member1,0);
    (v94).member1 = initArray((v94).member1, sizeof(uint32_t), getLength((at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member1));
    copyArray((v94).member1, (at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member1);
    (v94).member2 = initArray((v94).member2, sizeof(int32_t), getLength((at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member2));
    copyArray((v94).member2, (at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v5)).member2);
    (v6).member1 = setLength((v6).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v6).member1,0) = v91;
    (v6).member2 = setLength((v6).member2, sizeof(int32_t), v91);
    for (uint32_t v10 = 0; v10 < v92; v10 += 1)
    {
      at(int32_t,(v6).member2,v10) = at(int32_t,(v6).member2,v10);
    }
    for (uint32_t v13 = 0; v13 < v93; v13 += 1)
    {
      at(int32_t,(v6).member2,(v13 + v92)) = at(int32_t,(v94).member2,v13);
    }
  }
  (e0).member1 = initArray((e0).member1, sizeof(uint32_t), 1);
  copyArray((e0).member1, (v6).member1);
  (e0).member2 = initArray((e0).member2, sizeof(int32_t), getLength((v6).member2));
  copyArray((e0).member2, (v6).member2);
  v95 = at(uint32_t,(e0).member1,0);
  len2 = at(uint32_t,(*v0).member1,0);
  (v65).member1 = initArray((v65).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v65).member1,0) = 0;
  (v65).member2 = initArray((v65).member2, sizeof(int32_t), 0);
  for (uint32_t v64 = 0; v64 < len2; v64 += 1)
  {
    v96 = (at(uint32_t,(v65).member1,0) + at(uint32_t,(at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member1,0));
    v97 = at(uint32_t,(v65).member1,0);
    v98 = at(uint32_t,(at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member1,0);
    (v99).member1 = initArray((v99).member1, sizeof(uint32_t), getLength((at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member1));
    copyArray((v99).member1, (at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member1);
    (v99).member2 = initArray((v99).member2, sizeof(int32_t), getLength((at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member2));
    copyArray((v99).member2, (at(struct s_arr_unsignedS32_UD_arr_signedS32_UD,(*v0).member2,v64)).member2);
    (v65).member1 = setLength((v65).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v65).member1,0) = v96;
    (v65).member2 = setLength((v65).member2, sizeof(int32_t), v96);
    for (uint32_t v69 = 0; v69 < v97; v69 += 1)
    {
      at(int32_t,(v65).member2,v69) = at(int32_t,(v65).member2,v69);
    }
    for (uint32_t v72 = 0; v72 < v98; v72 += 1)
    {
      at(int32_t,(v65).member2,(v72 + v97)) = at(int32_t,(v99).member2,v72);
    }
  }
  (v100).member1 = initArray((v100).member1, sizeof(uint32_t), 1);
  copyArray((v100).member1, (v65).member1);
  (v100).member2 = initArray((v100).member2, sizeof(int32_t), getLength((v65).member2));
  copyArray((v100).member2, (v65).member2);
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v95;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v95);
  for (uint32_t v46 = 0; v46 < v95; v46 += 1)
  {
    at(int32_t,(*out).member2,v46) = at(int32_t,(v100).member2,v46);
  }
}
