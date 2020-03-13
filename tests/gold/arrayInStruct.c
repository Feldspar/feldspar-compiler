#include "arrayInStruct.h"


void arrayInStruct(struct array * v0, struct array * * out)
{
  struct s_2_unsignedS32_arr_unsignedS32 e11 = { .member1 = 0, .member2 = NULL };
  struct s_2_unsignedS32_arr_unsignedS32 v6 = { .member1 = 0, .member2 = NULL };
  uint32_t len12;
  struct array * e13 = NULL;
  bool v3;
  
  (e11).member1 = getLength(v0);
  (e11).member2 = initArray((e11).member2, sizeof(uint32_t), getLength(v0));
  (e11).member2 = copyArray((e11).member2, sizeof(uint32_t), v0);
  v3 = ((e11).member1 > 0);
  while (v3)
  {
    (v6).member1 = ((e11).member1 - 1);
    len12 = getLength((e11).member2);
    (v6).member2 = initArray((v6).member2, sizeof(uint32_t), len12);
    for (uint32_t v10 = 0; v10 < len12; v10 += 1)
    {
      at(uint32_t,(v6).member2,v10) = (at(uint32_t,(e11).member2,v10) + 5);
    }
    e13 = (e11).member2;
    e11 = v6;
    (v6).member2 = e13;
    v3 = ((e11).member1 > 0);
  }
  *out = initArray(*out, sizeof(uint32_t), getLength((e11).member2));
  *out = copyArray(*out, sizeof(uint32_t), (e11).member2);
}
