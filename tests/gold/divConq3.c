#include "divConq3.h"


void task_core0(uint32_t v501, uint32_t v631, struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct array * v963)
{
  uint32_t v640;
  uint32_t v644;
  struct s_2_arr_unsignedS32_arr_signedS32 e0 = { .member1 = NULL, .member2 = NULL };
  
  v640 = (v501 << 10);
  v644 = min(1024, (v631 - v640));
  (e0).member1 = setLength((e0).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e0).member1,0) = v644;
  (e0).member2 = initArray((e0).member2, sizeof(int32_t), v644);
  for (uint32_t v504 = 0; v504 < v644; v504 += 1)
  {
    at(int32_t,(e0).member2,v504) = (at(int32_t,(*v0).member2,(v504 + v640)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,v963,v501), &e0);
}

void task0(void * params)
{
  run4(task_core0, uint32_t, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, struct array *);
}

void task_core1(uint32_t v690, struct s_2_arr_unsignedS32_arr_signedS32 * v0, uint32_t v686, struct array * v968, uint32_t v448)
{
  struct s_2_arr_unsignedS32_arr_signedS32 e1 = { .member1 = NULL, .member2 = NULL };
  
  (e1).member1 = setLength((e1).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e1).member1,0) = v690;
  (e1).member2 = initArray((e1).member2, sizeof(int32_t), v690);
  for (uint32_t v451 = 0; v451 < v690; v451 += 1)
  {
    at(int32_t,(e1).member2,v451) = (at(int32_t,(*v0).member2,(v451 + v686)) + 1);
  }
  ivar_put(struct s_2_arr_unsignedS32_arr_signedS32, at(struct ivar,v968,v448), &e1);
}

void task1(void * params)
{
  run5(task_core1, uint32_t, struct s_2_arr_unsignedS32_arr_signedS32 *, uint32_t, struct array *, uint32_t);
}

void divConq3(struct s_2_arr_unsignedS32_arr_signedS32 * v0, struct s_2_arr_unsignedS32_arr_signedS32 * out)
{
  uint32_t v631;
  uint32_t v647;
  struct array * v963 = NULL;
  struct array * v964 = NULL;
  struct array * v968 = NULL;
  uint32_t v686;
  uint32_t v690;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v651 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v670 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v697 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v733 = { .member1 = NULL, .member2 = NULL };
  uint32_t len2;
  struct s_2_arr_unsignedS32_arr_signedS32 v431 = { .member1 = NULL, .member2 = NULL };
  uint32_t v969;
  struct ivar v970;
  struct ivar v971;
  struct ivar v698;
  struct s_2_arr_unsignedS32_arr_signedS32 v699 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v702 = { .member1 = NULL, .member2 = NULL };
  uint32_t v703;
  uint32_t v710;
  struct s_2_arr_unsignedS32_arr_signedS32 e3 = { .member1 = NULL, .member2 = NULL };
  struct array * e4 = NULL;
  struct array * e5 = NULL;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v752 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v771 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v798 = { .member1 = NULL, .member2 = NULL };
  uint32_t v835;
  struct s_2_arr_unsignedS32_arr_signedS32 e6 = { .member1 = NULL, .member2 = NULL };
  uint32_t len7;
  struct s_2_arr_unsignedS32_arr_signedS32 v222 = { .member1 = NULL, .member2 = NULL };
  uint32_t v974;
  struct ivar v975;
  struct ivar v976;
  struct ivar v799;
  struct s_2_arr_unsignedS32_arr_signedS32 v800 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v803 = { .member1 = NULL, .member2 = NULL };
  uint32_t v804;
  uint32_t v811;
  struct s_2_arr_unsignedS32_arr_signedS32 e8 = { .member1 = NULL, .member2 = NULL };
  struct array * e9 = NULL;
  struct array * e10 = NULL;
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v862 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v889 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_i_s_2_arr_unsignedS32_arr_signedS32 v916 = { .member1 = NULL, .member2 = NULL };
  uint32_t v961;
  struct s_2_arr_unsignedS32_arr_signedS32 e11 = { .member1 = NULL, .member2 = NULL };
  uint32_t len12;
  struct s_2_arr_unsignedS32_arr_signedS32 v117 = { .member1 = NULL, .member2 = NULL };
  uint32_t v979;
  struct ivar v980;
  struct ivar v981;
  struct ivar v917;
  struct s_2_arr_unsignedS32_arr_signedS32 v918 = { .member1 = NULL, .member2 = NULL };
  struct s_2_arr_unsignedS32_arr_signedS32 v921 = { .member1 = NULL, .member2 = NULL };
  uint32_t v922;
  uint32_t v929;
  struct s_2_arr_unsignedS32_arr_signedS32 e13 = { .member1 = NULL, .member2 = NULL };
  struct array * e14 = NULL;
  struct array * e15 = NULL;
  
  v631 = at(uint32_t,(*v0).member1,0);
  v647 = (v631 >> 10);
  v963 = initArray(v963, sizeof(struct ivar), v647);
  for (uint32_t v501 = 0; v501 < v647; v501 += 1)
  {
    ivar_init(&at(struct ivar,v963,v501));
    spawn4(task0, uint32_t, v501, uint32_t, v631, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, struct array *, v963);
  }
  v964 = setLength(v964, sizeof(uint32_t), 1);
  at(uint32_t,v964,0) = v647;
  v968 = initArray(v968, sizeof(struct ivar), v647);
  for (uint32_t v448 = 0; v448 < v647; v448 += 1)
  {
    v686 = (v448 << 10);
    v690 = min(1024, (v631 - v686));
    ivar_init(&at(struct ivar,v968,v448));
    spawn5(task1, uint32_t, v690, struct s_2_arr_unsignedS32_arr_signedS32 *, v0, uint32_t, v686, struct array *, v968, uint32_t, v448);
  }
  (v651).member1 = initArray((v651).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v651).member1, v964);
  (v651).member2 = initArray((v651).member2, sizeof(struct ivar), getLength(v963));
  copyArray((v651).member2, v963);
  (v670).member1 = initArray((v670).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v670).member1, v964);
  (v670).member2 = initArray((v670).member2, sizeof(struct ivar), getLength(v963));
  copyArray((v670).member2, v963);
  (v697).member1 = initArray((v697).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v697).member1, v964);
  (v697).member2 = initArray((v697).member2, sizeof(struct ivar), getLength(v968));
  copyArray((v697).member2, v968);
  len2 = at(uint32_t,v964,0);
  (v733).member1 = initArray((v733).member1, sizeof(uint32_t), 1);
  at(uint32_t,(v733).member1,0) = 0;
  (v733).member2 = initArray((v733).member2, sizeof(int32_t), 0);
  for (uint32_t v430 = 0; v430 < len2; v430 += 1)
  {
    v969 = at(uint32_t,(v733).member1,0);
    v970 = at(struct ivar,(v670).member2,v430);
    v971 = at(struct ivar,(v697).member2,v430);
    v698 = at(struct ivar,(v651).member2,v430);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v699, v698);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v702, v970);
    v703 = at(uint32_t,(v702).member1,0);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e3, v971);
    v710 = (v969 + at(uint32_t,(e3).member1,0));
    (v431).member1 = setLength((v431).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v431).member1,0) = v710;
    (v431).member2 = setLength((v431).member2, sizeof(int32_t), v710);
    for (uint32_t v459 = 0; v459 < v969; v459 += 1)
    {
      at(int32_t,(v431).member2,v459) = at(int32_t,(v733).member2,v459);
    }
    for (uint32_t v486 = 0; v486 < v703; v486 += 1)
    {
      at(int32_t,(v431).member2,(v486 + v969)) = at(int32_t,(v699).member2,v486);
    }
    e4 = (v733).member1;
    e5 = (v733).member2;
    v733 = v431;
    (v431).member1 = e4;
    (v431).member2 = e5;
  }
  (v752).member1 = initArray((v752).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v752).member1, v964);
  (v752).member2 = initArray((v752).member2, sizeof(struct ivar), getLength(v963));
  copyArray((v752).member2, v963);
  (v771).member1 = initArray((v771).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v771).member1, v964);
  (v771).member2 = initArray((v771).member2, sizeof(struct ivar), getLength(v963));
  copyArray((v771).member2, v963);
  (v798).member1 = initArray((v798).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v798).member1, v964);
  (v798).member2 = initArray((v798).member2, sizeof(struct ivar), getLength(v968));
  copyArray((v798).member2, v968);
  len7 = at(uint32_t,v964,0);
  (e6).member1 = initArray((e6).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e6).member1,0) = 0;
  (e6).member2 = initArray((e6).member2, sizeof(int32_t), 0);
  for (uint32_t v221 = 0; v221 < len7; v221 += 1)
  {
    v974 = at(uint32_t,(e6).member1,0);
    v975 = at(struct ivar,(v771).member2,v221);
    v976 = at(struct ivar,(v798).member2,v221);
    v799 = at(struct ivar,(v752).member2,v221);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v800, v799);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v803, v975);
    v804 = at(uint32_t,(v803).member1,0);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e8, v976);
    v811 = (v974 + at(uint32_t,(e8).member1,0));
    (v222).member1 = setLength((v222).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v222).member1,0) = v811;
    (v222).member2 = setLength((v222).member2, sizeof(int32_t), v811);
    for (uint32_t v250 = 0; v250 < v974; v250 += 1)
    {
      at(int32_t,(v222).member2,v250) = at(int32_t,(e6).member2,v250);
    }
    for (uint32_t v277 = 0; v277 < v804; v277 += 1)
    {
      at(int32_t,(v222).member2,(v277 + v974)) = at(int32_t,(v800).member2,v277);
    }
    e9 = (e6).member1;
    e10 = (e6).member2;
    e6 = v222;
    (v222).member1 = e9;
    (v222).member2 = e10;
  }
  v835 = at(uint32_t,(e6).member1,0);
  (v862).member1 = initArray((v862).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v862).member1, v964);
  (v862).member2 = initArray((v862).member2, sizeof(struct ivar), getLength(v968));
  copyArray((v862).member2, v968);
  (v889).member1 = initArray((v889).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v889).member1, v964);
  (v889).member2 = initArray((v889).member2, sizeof(struct ivar), getLength(v968));
  copyArray((v889).member2, v968);
  (v916).member1 = initArray((v916).member1, sizeof(uint32_t), getLength(v964));
  copyArray((v916).member1, v964);
  (v916).member2 = initArray((v916).member2, sizeof(struct ivar), getLength(v968));
  copyArray((v916).member2, v968);
  len12 = at(uint32_t,v964,0);
  (e11).member1 = initArray((e11).member1, sizeof(uint32_t), 1);
  at(uint32_t,(e11).member1,0) = 0;
  (e11).member2 = initArray((e11).member2, sizeof(int32_t), 0);
  for (uint32_t v116 = 0; v116 < len12; v116 += 1)
  {
    v979 = at(uint32_t,(e11).member1,0);
    v980 = at(struct ivar,(v889).member2,v116);
    v981 = at(struct ivar,(v916).member2,v116);
    v917 = at(struct ivar,(v862).member2,v116);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v918, v917);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &v921, v980);
    v922 = at(uint32_t,(v921).member1,0);
    ivar_get_nontask(struct s_2_arr_unsignedS32_arr_signedS32, &e13, v981);
    v929 = (v979 + at(uint32_t,(e13).member1,0));
    (v117).member1 = setLength((v117).member1, sizeof(uint32_t), 1);
    at(uint32_t,(v117).member1,0) = v929;
    (v117).member2 = setLength((v117).member2, sizeof(int32_t), v929);
    for (uint32_t v145 = 0; v145 < v979; v145 += 1)
    {
      at(int32_t,(v117).member2,v145) = at(int32_t,(e11).member2,v145);
    }
    for (uint32_t v172 = 0; v172 < v922; v172 += 1)
    {
      at(int32_t,(v117).member2,(v172 + v979)) = at(int32_t,(v918).member2,v172);
    }
    e14 = (e11).member1;
    e15 = (e11).member2;
    e11 = v117;
    (v117).member1 = e14;
    (v117).member2 = e15;
  }
  v961 = at(uint32_t,(e11).member1,0);
  (*out).member1 = setLength((*out).member1, sizeof(uint32_t), 1);
  at(uint32_t,(*out).member1,0) = v961;
  (*out).member2 = setLength((*out).member2, sizeof(int32_t), v961);
  for (uint32_t v316 = 0; v316 < v835; v316 += 1)
  {
    at(int32_t,(*out).member2,v316) = at(int32_t,(v733).member2,v316);
  }
  freeArray(v963);
  freeArray(v964);
  freeArray(v968);
  ivar_destroy(&v970);
  ivar_destroy(&v971);
  ivar_destroy(&v698);
  ivar_destroy(&v975);
  ivar_destroy(&v976);
  ivar_destroy(&v799);
  ivar_destroy(&v980);
  ivar_destroy(&v981);
  ivar_destroy(&v917);
}
