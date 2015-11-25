#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  struct s_2_signedS32_signedS32 v1 = { .member1 = 0, .member2 = 0 };
  struct s_3_signedS32_signedS32_signedS32 v2 = { .member1 = 0, .member2 = 0, .member3 = 0 };
  struct s_4_signedS32_signedS32_signedS32_signedS32 v3 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0 };
  struct s_5_signedS32_signedS32_signedS32_signedS32_signedS32 v4 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0 };
  struct s_6_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32 v5 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0, .member6 = 0 };
  struct s_7_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32 v6 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0, .member6 = 0, .member7 = 0 };
  int32_t v8;
  int32_t v9;
  int32_t v10;
  int32_t v11;
  int32_t v12;
  int32_t v13;
  int32_t v14;
  struct s_15_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32_signedS32 v7 = { .member1 = 0, .member2 = 0, .member3 = 0, .member4 = 0, .member5 = 0, .member6 = 0, .member7 = 0, .member8 = 0, .member9 = 0, .member10 = 0, .member11 = 0, .member12 = 0, .member13 = 0, .member14 = 0, .member15 = 0 };
  
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
  v8 = ((v6).member1 + (v6).member2);
  v9 = ((v6).member2 + (v6).member3);
  v10 = ((v6).member3 + (v6).member4);
  v11 = ((v6).member4 + (v6).member5);
  v12 = ((v6).member5 + (v6).member6);
  v13 = ((v6).member6 + (v6).member7);
  v14 = ((v6).member7 + (v6).member1);
  (v7).member1 = v8;
  (v7).member2 = v9;
  (v7).member3 = v10;
  (v7).member4 = v11;
  (v7).member5 = v12;
  (v7).member6 = v13;
  (v7).member7 = v14;
  (v7).member8 = v8;
  (v7).member9 = v9;
  (v7).member10 = v10;
  (v7).member11 = v11;
  (v7).member12 = v12;
  (v7).member13 = v13;
  (v7).member14 = v14;
  (v7).member15 = ((v6).member7 * (v6).member1);
  *out = (((((((((((((((v7).member1 + (v7).member2) + (v7).member3) + (v7).member4) + (v7).member5) + (v7).member6) + (v7).member7) + (v7).member8) + (v7).member9) + (v7).member10) + (v7).member11) + (v7).member12) + (v7).member13) + (v7).member14) + (v7).member15);
}
