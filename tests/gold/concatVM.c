#include "concatVM.h"


void concatVM(struct array * v1, struct array * * out)
{
  uint32_t len26;
  struct array * e27 = NULL;
  struct array * v8 = NULL;
  uint32_t v13;
  struct array * v10 = NULL;
  uint32_t v11;
  struct array * e28 = NULL;
  
  len26 = getLength(v1);
  e27 = *out;
  e27 = initArray(e27, sizeof(int32_t), 0);
  for (uint32_t v7 = 0; v7 < len26; v7 += 1)
  {
    v13 = getLength(e27);
    v10 = at(struct array *,v1,v7);
    v11 = getLength(v10);
    v8 = initArray(v8, sizeof(int32_t), (v13 + v11));
    for (uint32_t v19 = 0; v19 < v13; v19 += 1)
    {
      at(int32_t,v8,v19) = at(int32_t,e27,v19);
    }
    for (uint32_t v25 = 0; v25 < v11; v25 += 1)
    {
      at(int32_t,v8,(v25 + v13)) = at(int32_t,v10,v25);
    }
    e28 = e27;
    e27 = v8;
    v8 = e28;
  }
  *out = e27;
  freeArray(v8);
  freeArray(v10);
}
