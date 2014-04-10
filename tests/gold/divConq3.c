#include "divConq3.h"


void task_core1(struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v4, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634)
{
  uint32_t v632;
  uint32_t v633;
  struct s_2_arr_unsignedS32_arr_signedS32 e0 = { .member1 = NULL, .member2 = NULL };
  
  v632 = min(1024, (at(uint32_t,(*v0).member1,0) - (v4 << 10)));
  v633 = (v4 << 10);
  (e0).member1 = setLength((e0).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e0).member1,0) = v632;
  (e0).member2 = initArray((e0).member2, sizeof(int32_t), v632);
  for (uint32_t v7 = 0; v7 < v632; v7 += 1)
  {
    at(int32_t,(e0).member2,v7) = (at(int32_t,(*v0).member2,(v7 + v633)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v634).member2,v4), &e0);
}

void task1(void * params)
{
  run3(task_core1, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32);
}

void task_core12(struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v423, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643)
{
  uint32_t v641;
  uint32_t v642;
  struct s_2_arr_unsignedS32_arr_signedS32 e11 = { .member1 = NULL, .member2 = NULL };
  
  v641 = min(1024, (at(uint32_t,(*v0).member1,0) - (v423 << 10)));
  v642 = (v423 << 10);
  (e11).member1 = setLength((e11).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e11).member1,0) = v641;
  (e11).member2 = initArray((e11).member2, sizeof(int32_t), v641);
  for (uint32_t v426 = 0; v426 < v641; v426 += 1)
  {
    at(int32_t,(e11).member2,v426) = (at(int32_t,(*v0).member2,(v426 + v642)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,(v643).member2,v423), &e11);
}

void task12(void * params)
{
  run3(task_core12, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32);
}

void divConq3(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v639;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v634 = { .member1 = NULL, .member2 = NULL };
  uint32_t v631;
  struct s_2_arr_unsignedS32_arr_signedS32 e2 = { .member1 = NULL, .member2 = NULL };
  uint32_t len3;
  struct s_2_arr_unsignedS32_arr_signedS32 v12 = { .member1 = NULL, .member2 = NULL };
  uint32_t v635;
  struct s_2_arr_unsignedS32_arr_signedS32 e4 = { .member1 = NULL, .member2 = NULL };
  struct ivar e5;
  uint32_t v636;
  uint32_t v637;
  struct s_2_arr_unsignedS32_arr_signedS32 e6 = { .member1 = NULL, .member2 = NULL };
  struct ivar e7;
  struct s_2_arr_unsignedS32_arr_signedS32 v638 = { .member1 = NULL, .member2 = NULL };
  struct ivar e8;
  struct array * e9 = NULL;
  struct array * e10 = NULL;
  struct s_2_arr_unsignedS32_arr_signedS32 v648 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v643 = { .member1 = NULL, .member2 = NULL };
  uint32_t v640;
  uint32_t len13;
  struct s_2_arr_unsignedS32_arr_signedS32 v431 = { .member1 = NULL, .member2 = NULL };
  uint32_t v644;
  struct s_2_arr_unsignedS32_arr_signedS32 e14 = { .member1 = NULL, .member2 = NULL };
  struct ivar e15;
  uint32_t v645;
  uint32_t v646;
  struct s_2_arr_unsignedS32_arr_signedS32 e16 = { .member1 = NULL, .member2 = NULL };
  struct ivar e17;
  struct s_2_arr_unsignedS32_arr_signedS32 v647 = { .member1 = NULL, .member2 = NULL };
  struct ivar e18;
  struct array * e19 = NULL;
  struct array * e20 = NULL;
  
  v631 = (at(uint32_t,(*v0).member1,0) >> 10);
  (v634).member1 = setLength((v634).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v634).member1,0) = v631;
  (v634).member2 = initArray((v634).member2, sizeof(struct ivar), v631);
  for (uint32_t v4 = 0; v4 < v631; v4 += 1)
  {
    ivar_init(&at(struct ivar,(v634).member2,v4));
    spawn3(task1, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v4, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v634);
  }
  len3 = at(uint32_t,(v634).member1,0);
  (e2).member1 = initArray((e2).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e2).member1,0) = 0;
  (e2).member2 = initArray((e2).member2, sizeof(int32_t), 0);
  for (uint32_t v11 = 0; v11 < len3; v11 += 1)
  {
    e5 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e4, e5);
    v635 = (at(uint32_t,(e2).member1,0) + at(uint32_t,(e4).member1,0));
    v636 = at(uint32_t,(e2).member1,0);
    e7 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e6, e7);
    v637 = at(uint32_t,(e6).member1,0);
    e8 = at(struct ivar,(v634).member2,v11);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v638, e8);
    (v12).member1 = setLength((v12).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v12).member1,0) = v635;
    (v12).member2 = setLength((v12).member2, sizeof(int32_t), v635);
    for (uint32_t v40 = 0; v40 < v636; v40 += 1)
    {
      at(int32_t,(v12).member2,v40) = at(int32_t,(e2).member2,v40);
    }
    for (uint32_t v67 = 0; v67 < v637; v67 += 1)
    {
      at(int32_t,(v12).member2,(v67 + v636)) = at(int32_t,(v638).member2,v67);
    }
    e9 = (e2).member1;
    e10 = (e2).member2;
    e2 = v12;
    (v12).member1 = e9;
    (v12).member2 = e10;
  }
  v639 = at(uint32_t,(e2).member1,0);
  v640 = (at(uint32_t,(*v0).member1,0) >> 10);
  (v643).member1 = setLength((v643).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v643).member1,0) = v640;
  (v643).member2 = initArray((v643).member2, sizeof(struct ivar), v640);
  for (uint32_t v423 = 0; v423 < v640; v423 += 1)
  {
    ivar_init(&at(struct ivar,(v643).member2,v423));
    spawn3(task12, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v423, struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32, v643);
  }
  len13 = at(uint32_t,(v643).member1,0);
  (v648).member1 = initArray((v648).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v648).member1,0) = 0;
  (v648).member2 = initArray((v648).member2, sizeof(int32_t), 0);
  for (uint32_t v430 = 0; v430 < len13; v430 += 1)
  {
    e15 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e14, e15);
    v644 = (at(uint32_t,(v648).member1,0) + at(uint32_t,(e14).member1,0));
    v645 = at(uint32_t,(v648).member1,0);
    e17 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e16, e17);
    v646 = at(uint32_t,(e16).member1,0);
    e18 = at(struct ivar,(v643).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v647, e18);
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
    e19 = (v648).member1;
    e20 = (v648).member2;
    v648 = v431;
    (v431).member1 = e19;
    (v431).member2 = e20;
  }
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v639;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v639);
  for (uint32_t v316 = 0; v316 < v639; v316 += 1)
  {
    at(int32_t,(*out).member2,v316) = at(int32_t,(v648).member2,v316);
  }
  ivar_destroy(&e5);
  ivar_destroy(&e7);
  ivar_destroy(&e8);
  ivar_destroy(&e15);
  ivar_destroy(&e17);
  ivar_destroy(&e18);
}
