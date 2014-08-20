#include "pairParam2.h"


void pairParam2(struct s_2_signedS16_signedS16 * v0, struct s_2_s_2_signedS16_signedS16_s_2_signedS16_signedS16 * out)
{
  int16_t i0_1;
  int16_t i0_2;
  int16_t o1_1_1;
  int16_t o1_1_2;
  int16_t o1_2_1;
  int16_t o1_2_2;
  
  i0_1 = (*v0).member1;
  i0_2 = (*v0).member2;
  o1_1_1 = i0_1;
  o1_1_2 = i0_2;
  o1_2_1 = i0_1;
  o1_2_2 = i0_2;
  ((*out).member1).member1 = o1_1_1;
  ((*out).member1).member2 = o1_1_2;
  ((*out).member2).member1 = o1_2_1;
  ((*out).member2).member2 = o1_2_2;
}
