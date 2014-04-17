#include "divConq3.h"


void task_core0(uint32_t v632, struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v633, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634, uint32_t v4)
{
  struct s_2_arr_unsignedS32_arr_signedS32 e0 = { .member1 = NULL, .member2 = NULL };
  
  (e0).member1 = setLength((e0).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e0).member1,0) = v632;
  (e0).member2 = initArray((e0).member2, sizeof(int32_t), v632);
  for (uint32_t v7 = 0; v7 < v632; v7 += 1)
  {
    at(int32_t,(e0).member2,v7) = (at(int32_t,(*v0).member2,(v7 + v633)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v634).member2,v4), &e0);
}

void task0(void * params)
{
  run5(task_core0, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, uint32_t);
}

void task_core1(uint32_t v641, struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v642, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643, uint32_t v423)
{
  struct s_2_arr_unsignedS32_arr_signedS32 e10 = { .member1 = NULL, .member2 = NULL };
  
  (e10).member1 = setLength((e10).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e10).member1,0) = v641;
  (e10).member2 = initArray((e10).member2, sizeof(int32_t), v641);
  for (uint32_t v426 = 0; v426 < v641; v426 += 1)
  {
    at(int32_t,(e10).member2,v426) = (at(int32_t,(*v0).member2,(v426 + v642)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v643).member2,v423), &e10);
}

void task1(void * params)
{
  run5(task_core1, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, uint32_t);
}

void divConq3(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v639;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634 = { .member1 = NULL, .member2 = NULL };
  uint32_t v631;
  uint32_t v632;
  uint32_t v633;
  struct s_2_arr_unsignedS32_arr_signedS32 e1 = { .member1 = NULL, .member2 = NULL };
  uint32_t len2;
  struct s_2_arr_unsignedS32_arr_signedS32 v12 = { .member1 = NULL, .member2 = NULL };
  uint32_t v635;
  struct s_2_arr_unsignedS32_arr_signedS32 e3 = { .member1 = NULL, .member2 = NULL };
  struct ivar e4;
  uint32_t v636;
  uint32_t v637;
  struct s_2_arr_unsignedS32_arr_signedS32 e5 = { .member1 = NULL, .member2 = NULL };
  struct ivar e6;
  struct s_2_arr_unsignedS32_arr_signedS32 v638 = { .member1 = NULL, .member2 = NULL };
  struct ivar e7;
  struct array * e8 = NULL;
  struct array * e9 = NULL;
  struct s_2_arr_unsignedS32_arr_signedS32 v648 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643 = { .member1 = NULL, .member2 = NULL };
  uint32_t v640;
  uint32_t v641;
  uint32_t v642;
  uint32_t len11;
  struct s_2_arr_unsignedS32_arr_signedS32 v431 = { .member1 = NULL, .member2 = NULL };
  uint32_t v644;
  struct s_2_arr_unsignedS32_arr_signedS32 e12 = { .member1 = NULL, .member2 = NULL };
  struct ivar e13;
  uint32_t v645;
  uint32_t v646;
  struct s_2_arr_unsignedS32_arr_signedS32 e14 = { .member1 = NULL, .member2 = NULL };
  struct ivar e15;
  struct s_2_arr_unsignedS32_arr_signedS32 v647 = { .member1 = NULL, .member2 = NULL };
  struct ivar e16;
  struct array * e17 = NULL;
  struct array * e18 = NULL;
  
  v631 = (at(uint32_t,(*v0).member1,0) >> 10);
  (v634).member1 = setLength((v634).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v634).member1,0) = v631;
  (v634).member2 = initArray((v634).member2, sizeof(struct ivar), v631);
  for (uint32_t v4 = 0; v4 < v631; v4 += 1)
  {
    v632 = min(1024, (at(uint32_t,(*v0).member1,0) - (v4 << 10)));
    v633 = (v4 << 10);
    ivar_init(&at(struct ivar,(v634).member2,v4));
    spawn5(task0, uint32_t, v632, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v633, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v634, uint32_t, v4);
  }
  len2 = at(uint32_t,(v634).member1,0);
  (e1).member1 = initArray((e1).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e1).member1,0) = 0;
  (e1).member2 = initArray((e1).member2, sizeof(int32_t), 0);
  for (uint32_t v11 = 0; v11 < len2; v11 += 1)
  {
    e4 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e3, e4);
    v635 = (at(uint32_t,(e1).member1,0) + at(uint32_t,(e3).member1,0));
    v636 = at(uint32_t,(e1).member1,0);
    e6 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e5, e6);
    v637 = at(uint32_t,(e5).member1,0);
    e7 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v638, e7);
    (v12).member1 = setLength((v12).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v12).member1,0) = v635;
    (v12).member2 = setLength((v12).member2, sizeof(int32_t), v635);
    for (uint32_t v40 = 0; v40 < v636; v40 += 1)
    {
      at(int32_t,(v12).member2,v40) = at(int32_t,(e1).member2,v40);
    }
    for (uint32_t v67 = 0; v67 < v637; v67 += 1)
    {
      at(int32_t,(v12).member2,(v67 + v636)) = at(int32_t,(v638).member2,v67);
    }
    e8 = (e1).member1;
    e9 = (e1).member2;
    e1 = v12;
    (v12).member1 = e8;
    (v12).member2 = e9;
  }
  v639 = at(uint32_t,(e1).member1,0);
  v640 = (at(uint32_t,(*v0).member1,0) >> 10);
  (v643).member1 = setLength((v643).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v643).member1,0) = v640;
  (v643).member2 = initArray((v643).member2, sizeof(struct ivar), v640);
  for (uint32_t v423 = 0; v423 < v640; v423 += 1)
  {
    v641 = min(1024, (at(uint32_t,(*v0).member1,0) - (v423 << 10)));
    v642 = (v423 << 10);
    ivar_init(&at(struct ivar,(v643).member2,v423));
    spawn5(task1, uint32_t, v641, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v642, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v643, uint32_t, v423);
  }
  len11 = at(uint32_t,(v643).member1,0);
  (v648).member1 = initArray((v648).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v648).member1,0) = 0;
  (v648).member2 = initArray((v648).member2, sizeof(int32_t), 0);
  for (uint32_t v430 = 0; v430 < len11; v430 += 1)
  {
    e13 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e12, e13);
    v644 = (at(uint32_t,(v648).member1,0) + at(uint32_t,(e12).member1,0));
    v645 = at(uint32_t,(v648).member1,0);
    e15 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e14, e15);
    v646 = at(uint32_t,(e14).member1,0);
    e16 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v647, e16);
    (v431).member1 = setLength((v431).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v431).member1,0) = v644;
    (v431).member2 = setLength((v431).member2, sizeof(int32_t), v644);
    for (uint32_t v459 = 0; v459 < v645; v459 += 1)
    {
      at(int32_t,(v431).member2,v459) = at(int32_t,(v648).member2,v459);
    }
    for (uint32_t v486 = 0; v486 < v646; v486 += 1)
    {
      at(int32_t,(v431).member2,(v486 + v645)) = at(int32_t,(v647).member2,v486);
    }
    e17 = (v648).member1;
    e18 = (v648).member2;
    v648 = v431;
    (v431).member1 = e17;
    (v431).member2 = e18;
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v639;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v639);
  for (uint32_t v316 = 0; v316 < v639; v316 += 1)
  {
    at(int32_t,(*out).member2,v316) = at(int32_t,(v648).member2,v316);
  }
  ivar_destroy(&e4);
  ivar_destroy(&e6);
  ivar_destroy(&e7);
  ivar_destroy(&e13);
  ivar_destroy(&e15);
  ivar_destroy(&e16);
}
