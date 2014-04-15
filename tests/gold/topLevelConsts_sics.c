#include "topLevelConsts_sics.h"


uint32_t v2[] = {2, 3, 4, 5, 6};

uint32_t v3[] = {1, 2, 3, 4, 5};

void topLevelConsts__sics(uint32_t v0, uint32_t v1, uint32_t * out)
{
  uint32_t v5;
  
  v5 = (v1 + 5);
  if ((v0 < 5))
  {
    *out = v2[v5];
  }
  else
  {
    *out = v3[v5];
  }
}
