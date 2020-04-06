#include "arrayInStruct.h"


void arrayInStruct(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out)
{
  struct s_2_unsignedS32_awl_unsignedS32 e11 = { .member1 = 0, .member2 = { .buffer = NULL, .length = 0 } };
  struct s_2_unsignedS32_awl_unsignedS32 v6 = { .member1 = 0, .member2 = { .buffer = NULL, .length = 0 } };
  uint32_t len12;
  struct awl_unsignedS32 e13 = { .buffer = NULL, .length = 0 };
  bool v3;
  
  (e11).member1 = (*v0).length;
  ((e11).member2).buffer = initCopyArray(((e11).member2).buffer, ((e11).member2).length, sizeof(uint32_t), (*v0).buffer, (*v0).length);
  ((e11).member2).length = (*v0).length;
  v3 = ((e11).member1 > 0);
  while (v3)
  {
    (v6).member1 = ((e11).member1 - 1);
    len12 = ((e11).member2).length;
    ((v6).member2).buffer = initArray(((v6).member2).buffer, ((v6).member2).length, sizeof(uint32_t), len12);
    ((v6).member2).length = len12;
    for (uint32_t v10 = 0; v10 < len12; v10 += 1)
    {
      ((v6).member2).buffer[v10] = (((e11).member2).buffer[v10] + 5);
    }
    e13 = (e11).member2;
    e11 = v6;
    (v6).member2 = e13;
    v3 = ((e11).member1 > 0);
  }
  (*out).buffer = initCopyArray((*out).buffer, (*out).length, sizeof(uint32_t), ((e11).member2).buffer, ((e11).member2).length);
  (*out).length = ((e11).member2).length;
  freeArray(((e11).member2).buffer);
  freeArray(((v6).member2).buffer);
}
