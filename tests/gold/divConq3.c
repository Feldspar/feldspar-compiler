#include "divConq3.h"


void task_core0(uint32_t v632, struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v651, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634, uint32_t v4)
{
  struct s_2_arr_unsignedS32_arr_signedS32 e0 = { .member1 = NULL, .member2 = NULL };
  
  (e0).member1 = setLength((e0).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e0).member1,0) = v632;
  (e0).member2 = initArray((e0).member2, sizeof(int32_t), v632);
  for (uint32_t v7 = 0; v7 < v632; v7 += 1)
  {
    at(int32_t,(e0).member2,v7) = (at(int32_t,(*v0).member2,(v7 + v651)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v634).member2,v4), &e0);
}

void task0(void * params)
{
  run5(task_core0, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, uint32_t);
}

void task_core1(uint32_t v641, struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v654, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643, uint32_t v423)
{
  struct s_2_arr_unsignedS32_arr_signedS32 e6 = { .member1 = NULL, .member2 = NULL };
  
  (e6).member1 = setLength((e6).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e6).member1,0) = v641;
  (e6).member2 = initArray((e6).member2, sizeof(int32_t), v641);
  for (uint32_t v426 = 0; v426 < v641; v426 += 1)
  {
    at(int32_t,(e6).member2,v426) = (at(int32_t,(*v0).member2,(v426 + v654)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v643).member2,v423), &e6);
}

void task1(void * params)
{
  run5(task_core1, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, uint32_t);
}

void divConq3(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v657;
  uint32_t v649;
  uint32_t v639;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634 = { .member1 = NULL, .member2 = NULL };
  uint32_t v651;
  uint32_t v632;
  struct s_2_arr_unsignedS32_arr_signedS32 e1 = { .member1 = NULL, .member2 = NULL };
  uint32_t len2;
  struct s_2_arr_unsignedS32_arr_signedS32 v12 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v658 = { .member1 = NULL, .member2 = NULL };
  struct ivar e3;
  uint32_t v652;
  uint32_t v653;
  uint32_t v635;
  struct array * e4 = NULL;
  struct array * e5 = NULL;
  struct s_2_arr_unsignedS32_arr_signedS32 v648 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643 = { .member1 = NULL, .member2 = NULL };
  uint32_t v654;
  uint32_t v641;
  uint32_t len7;
  struct s_2_arr_unsignedS32_arr_signedS32 v431 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v659 = { .member1 = NULL, .member2 = NULL };
  struct ivar e8;
  uint32_t v655;
  uint32_t v656;
  uint32_t v644;
  struct array * e9 = NULL;
  struct array * e10 = NULL;
  
  v657 = at(uint32_t,(*v0).member1,0);
  v649 = (v657 >> 10);
  (v634).member1 = setLength((v634).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v634).member1,0) = v649;
  (v634).member2 = initArray((v634).member2, sizeof(struct ivar), v649);
  for (uint32_t v4 = 0; v4 < v649; v4 += 1)
  {
    v651 = (v4 << 10);
    v632 = min(1024, (v657 - v651));
    ivar_init(&at(struct ivar,(v634).member2,v4));
    spawn5(task0, uint32_t, v632, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v651, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v634, uint32_t, v4);
  }
  len2 = at(uint32_t,(v634).member1,0);
  (e1).member1 = initArray((e1).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e1).member1,0) = 0;
  (e1).member2 = initArray((e1).member2, sizeof(int32_t), 0);
  for (uint32_t v11 = 0; v11 < len2; v11 += 1)
  {
    e3 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v658, e3);
    v652 = at(uint32_t,(e1).member1,0);
    v653 = at(uint32_t,(v658).member1,0);
    v635 = (v652 + v653);
    (v12).member1 = setLength((v12).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v12).member1,0) = v635;
    (v12).member2 = setLength((v12).member2, sizeof(int32_t), v635);
    for (uint32_t v40 = 0; v40 < v652; v40 += 1)
    {
      at(int32_t,(v12).member2,v40) = at(int32_t,(e1).member2,v40);
    }
    for (uint32_t v67 = 0; v67 < v653; v67 += 1)
    {
      at(int32_t,(v12).member2,(v67 + v652)) = at(int32_t,(v658).member2,v67);
    }
    e4 = (e1).member1;
    e5 = (e1).member2;
    e1 = v12;
    (v12).member1 = e4;
    (v12).member2 = e5;
  }
  v639 = at(uint32_t,(e1).member1,0);
  (v643).member1 = setLength((v643).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v643).member1,0) = v649;
  (v643).member2 = initArray((v643).member2, sizeof(struct ivar), v649);
  for (uint32_t v423 = 0; v423 < v649; v423 += 1)
  {
    v654 = (v423 << 10);
    v641 = min(1024, (v657 - v654));
    ivar_init(&at(struct ivar,(v643).member2,v423));
    spawn5(task1, uint32_t, v641, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v654, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v643, uint32_t, v423);
  }
  len7 = at(uint32_t,(v643).member1,0);
  (v648).member1 = initArray((v648).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v648).member1,0) = 0;
  (v648).member2 = initArray((v648).member2, sizeof(int32_t), 0);
  for (uint32_t v430 = 0; v430 < len7; v430 += 1)
  {
    e8 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v659, e8);
    v655 = at(uint32_t,(v648).member1,0);
    v656 = at(uint32_t,(v659).member1,0);
    v644 = (v655 + v656);
    (v431).member1 = setLength((v431).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v431).member1,0) = v644;
    (v431).member2 = setLength((v431).member2, sizeof(int32_t), v644);
    for (uint32_t v459 = 0; v459 < v655; v459 += 1)
    {
      at(int32_t,(v431).member2,v459) = at(int32_t,(v648).member2,v459);
    }
    for (uint32_t v486 = 0; v486 < v656; v486 += 1)
    {
      at(int32_t,(v431).member2,(v486 + v655)) = at(int32_t,(v659).member2,v486);
    }
    e9 = (v648).member1;
    e10 = (v648).member2;
    v648 = v431;
    (v431).member1 = e9;
    (v431).member2 = e10;
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v639;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v639);
  for (uint32_t v316 = 0; v316 < v639; v316 += 1)
  {
    at(int32_t,(*out).member2,v316) = at(int32_t,(v648).member2,v316);
  }
  ivar_destroy(&e3);
  ivar_destroy(&e8);
}
