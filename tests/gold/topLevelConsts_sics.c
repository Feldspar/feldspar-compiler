#include "topLevelConsts_sics.h"


void topLevelConsts__sics(uint32_t v0, uint32_t v1, uint32_t * out)
{
  uint32_t v5;
  
  v5 = (v1 + 5);
  if ((v0 < 5))
  {
    *out = ((uint32_t[]){2, 3, 4, 5, 6})[v5];
  }
  else
  {
    *out = ((uint32_t[]){1, 2, 3, 4, 5})[v5];
  }
}
