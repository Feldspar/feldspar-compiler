#include "issue128_ex1.h"


void issue128__ex1(uint32_t v0, uint32_t * out)
{
  bool v2;
  
  v2 = (1 == v0);
  if (v2)
  {
    if (v2)
    {
      *out = 10;
    }
    else
    {
      *out = 45;
    }
  }
  else
  {
    *out = v0;
  }
}
