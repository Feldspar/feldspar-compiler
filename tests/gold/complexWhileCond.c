#include "complexWhileCond.h"


void complexWhileCond(int32_t v0, struct s_2_signedS32_signedS32 * out)
{
  struct s_2_signedS32_signedS32 e6 = { .member1 = 0, .member2 = 0 };
  struct s_2_signedS32_signedS32 v2 = { .member1 = 0, .member2 = 0 };
  int32_t v5;
  bool v1;
  
  (e6).member1 = 0;
  (e6).member2 = v0;
  v5 = ((e6).member2 - (e6).member1);
  v1 = (((e6).member1 * (e6).member1) != (v5 * v5));
  while (v1)
  {
    (v2).member1 = ((e6).member1 + 1);
    (v2).member2 = (e6).member2;
    e6 = v2;
    v5 = ((e6).member2 - (e6).member1);
    v1 = (((e6).member1 * (e6).member1) != (v5 * v5));
  }
  *out = e6;
}
