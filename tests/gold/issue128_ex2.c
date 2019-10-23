#include "issue128_ex2.h"


void issue128__ex2(uint32_t v0, uint32_t * out)
{
  uint32_t v1;
  
  switch (v0)
  {
    case 1:
      v1 = 20;
      break;
    default:
      v1 = 45;
      break;
  }
  if ((2 == v0))
  {
    *out = v1;
  }
  else
  {
    *out = v0;
  }
}
