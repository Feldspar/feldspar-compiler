#include "concatVM.h"


void concatVM(struct array * v1, struct array * * out)
{
  uint32_t len22;
  struct array * e23 = NULL;
  struct array * v7 = NULL;
  uint32_t v12;
  struct array * v9 = NULL;
  uint32_t v10;
  uint32_t len24;
  struct array * e25 = NULL;
  
  len22 = getLength(v1);
  e23 = *out;
  e23 = initArray(e23, sizeof(int32_t), 0);
  for (uint32_t v6 = 0; v6 < len22; v6 += 1)
  {
    v12 = getLength(e23);
    v9 = at(struct array *,v1,v6);
    v10 = getLength(v9);
    len24 = (v12 + v10);
    v7 = initArray(v7, sizeof(int32_t), len24);
    for (uint32_t v17 = 0; v17 < v12; v17 += 1)
    {
      at(int32_t,v7,v17) = at(int32_t,e23,v17);
    }
    for (uint32_t v21 = 0; v21 < v10; v21 += 1)
    {
      at(int32_t,v7,(v21 + v12)) = at(int32_t,v9,v21);
    }
    e25 = e23;
    e23 = v7;
    v7 = e25;
  }
  *out = e23;
  freeArray(v7);
  freeArray(v9);
}
