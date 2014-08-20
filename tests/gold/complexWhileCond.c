#include "complexWhileCond.h"


void complexWhileCond(int32_t v0, struct s_2_signedS32_signedS32 * out)
{
  int32_t o0_1;
  int32_t o0_2;
  int32_t v5;
  bool v1;
  
  o0_1 = 0;
  o0_2 = v0;
  v5 = (o0_2 - o0_1);
  v1 = ((o0_1 * o0_1) != (v5 * v5));
  while (v1)
  {
    o0_1 = (o0_1 + 1);
    o0_1 = o0_1;
    o0_2 = o0_2;
    v5 = (o0_2 - o0_1);
    v1 = ((o0_1 * o0_1) != (v5 * v5));
  }
  (*out).member1 = o0_1;
  (*out).member2 = o0_2;
}
