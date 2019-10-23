#include "issue128_ex1.h"


void issue128__ex1(uint32_t v0, uint32_t * out)
{
  bool v2;
  uint32_t v1;
  
  v2 = (1 == v0);
  if (v2)
  {
    v1 = 10;
  }
  else
  {
    v1 = 45;
  }
  if (v2)
  {
    *out = v1;
  }
  else
  {
    *out = v0;
  }
}
