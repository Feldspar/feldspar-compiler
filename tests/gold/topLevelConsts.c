#include "topLevelConsts.h"


void topLevelConsts(uint32_t v0, uint32_t v1, uint32_t * out)
{
  uint32_t v2;
  
  v2 = (v1 + 5);
  if ((v0 < 5))
  {
    *out = ((uint32_t[]){2, 3, 4, 5, 6})[v2];
  }
  else
  {
    *out = ((uint32_t[]){1, 2, 3, 4, 5})[v2];
  }
}
