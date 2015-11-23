#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  struct s_2_signedS32_signedS32 v1 = { .member1 = 0, .member2 = 0 };
  struct s_3_signedS32_signedS32_signedS32 v2 = { .member1 = 0, .member2 = 0, .member3 = 0 };
  struct s_4_signedS32_signedS32_signedS32_signedS32 v3 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0 };
  struct s_5_signedS32_signedS32_signedS32_signedS32_signedS32 v4 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0 };
  struct s_6_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32 v5 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0, .member6 = 0 };
  struct s_7_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32 v6 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0, .member6 = 0, .member7 = 0 };
  
  (v1).member1 = v0;
  (v1).member2 = (v0 * 3);
  (v2).member1 = ((v1).member1 + (v1).member2);
  (v2).member2 = ((v1).member2 + (v1).member1);
  (v2).member3 = (v1).member2;
  (v3).member1 = ((v2).member1 + (v2).member2);
  (v3).member2 = ((v2).member2 + (v2).member3);
  (v3).member3 = ((v2).member3 + (v2).member1);
  (v3).member4 = (v2).member3;
  (v4).member1 = ((v3).member1 + (v3).member2);
  (v4).member2 = ((v3).member2 + (v3).member3);
  (v4).member3 = ((v3).member3 + (v3).member4);
  (v4).member4 = ((v3).member4 + (v3).member1);
  (v4).member5 = (v3).member4;
  (v5).member1 = ((v4).member1 + (v4).member2);
  (v5).member2 = ((v4).member2 + (v4).member3);
  (v5).member3 = ((v4).member3 + (v4).member4);
  (v5).member4 = ((v4).member4 + (v4).member5);
  (v5).member5 = ((v4).member5 + (v4).member1);
  (v5).member6 = (v4).member5;
  (v6).member1 = ((v5).member1 + (v5).member2);
  (v6).member2 = ((v5).member2 + (v5).member3);
  (v6).member3 = ((v5).member3 + (v5).member4);
  (v6).member4 = ((v5).member4 + (v5).member5);
  (v6).member5 = ((v5).member5 + (v5).member6);
  (v6).member6 = ((v5).member6 + (v5).member1);
  (v6).member7 = (v5).member6;
  *out = (((((((v6).member1 + (v6).member2) + (v6).member3) + (v6).member4) + (v6).member5) + (v6).member6) + (v6).member7);
}
