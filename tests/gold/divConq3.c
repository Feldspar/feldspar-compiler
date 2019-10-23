#include "divConq3.h"


void task_core0(uint32_t v186, struct array * v0, uint32_t v204, struct array * v188, uint32_t v1)
{
  struct array * e209 = NULL;
  
  e209 = initArray(e209, sizeof(int32_t), v186);
  for (uint32_t v2 = 0; v2 < v186; v2 += 1)
  {
    at(int32_t,e209,v2) = (at(int32_t,v0,(v2 + v204)) + 1);
  }
  ivar_put_array(at(struct ivar,v188,v1), e209);
}

void task0(void * params)
{
  run5(task_core0, uint32_t, struct array *, uint32_t, struct array *, uint32_t);
}

void task_core1(uint32_t v194, struct array * v0, uint32_t v206, struct array * v196, uint32_t v111)
{
  struct array * e213 = NULL;
  
  e213 = initArray(e213, sizeof(int32_t), v194);
  for (uint32_t v112 = 0; v112 < v194; v112 += 1)
  {
    at(int32_t,e213,v112) = (at(int32_t,v0,(v112 + v206)) + 1);
  }
  ivar_put_array(at(struct ivar,v196,v111), e213);
}

void task1(void * params)
{
  run5(task_core1, uint32_t, struct array *, uint32_t, struct array *, uint32_t);
}

void divConq3(struct array * v0, struct array * * out)
{
  uint32_t v208;
  uint32_t v201;
  struct array * v188 = NULL;
  uint32_t v204;
  uint32_t v186;
  uint32_t v192;
  struct array * e210 = NULL;
  struct array * v5 = NULL;
  struct array * v205 = NULL;
  struct ivar e211;
  uint32_t v189;
  uint32_t v190;
  struct array * e212 = NULL;
  struct array * v196 = NULL;
  uint32_t v206;
  uint32_t v194;
  struct array * v200 = NULL;
  struct array * v115 = NULL;
  struct array * v207 = NULL;
  struct ivar e214;
  uint32_t v197;
  uint32_t v198;
  struct array * e215 = NULL;
  
  taskpool_init(4, 4, 4);
  v208 = getLength(v0);
  v201 = (v208 >> 10);
  v188 = initArray(v188, sizeof(struct ivar), v201);
  for (uint32_t v1 = 0; v1 < v201; v1 += 1)
  {
    v204 = (v1 << 10);
    v186 = min(1024, (v208 - v204));
    ivar_init(&at(struct ivar,v188,v1));
    spawn5(task0, uint32_t, v186, struct array *, v0, uint32_t, v204, struct array *, v188, uint32_t, v1);
  }
  e210 = initArray(e210, sizeof(int32_t), 0);
  for (uint32_t v4 = 0; v4 < v201; v4 += 1)
  {
    e211 = at(struct ivar,v188,v4);
    v205 = ivar_get_array_nontask(v205, e211);
    v189 = getLength(e210);
    v190 = getLength(v205);
    v5 = initArray(v5, sizeof(int32_t), (v189 + v190));
    for (uint32_t v11 = 0; v11 < v189; v11 += 1)
    {
      at(int32_t,v5,v11) = at(int32_t,e210,v11);
    }
    for (uint32_t v22 = 0; v22 < v190; v22 += 1)
    {
      at(int32_t,v5,(v22 + v189)) = at(int32_t,v205,v22);
    }
    e212 = e210;
    e210 = v5;
    v5 = e212;
  }
  v192 = getLength(e210);
  v196 = initArray(v196, sizeof(struct ivar), v201);
  for (uint32_t v111 = 0; v111 < v201; v111 += 1)
  {
    v206 = (v111 << 10);
    v194 = min(1024, (v208 - v206));
    ivar_init(&at(struct ivar,v196,v111));
    spawn5(task1, uint32_t, v194, struct array *, v0, uint32_t, v206, struct array *, v196, uint32_t, v111);
  }
  v200 = initArray(v200, sizeof(int32_t), 0);
  for (uint32_t v114 = 0; v114 < v201; v114 += 1)
  {
    e214 = at(struct ivar,v196,v114);
    v207 = ivar_get_array_nontask(v207, e214);
    v197 = getLength(v200);
    v198 = getLength(v207);
    v115 = initArray(v115, sizeof(int32_t), (v197 + v198));
    for (uint32_t v121 = 0; v121 < v197; v121 += 1)
    {
      at(int32_t,v115,v121) = at(int32_t,v200,v121);
    }
    for (uint32_t v132 = 0; v132 < v198; v132 += 1)
    {
      at(int32_t,v115,(v132 + v197)) = at(int32_t,v207,v132);
    }
    e215 = v200;
    v200 = v115;
    v115 = e215;
  }
  *out = initArray(*out, sizeof(int32_t), v192);
  for (uint32_t v74 = 0; v74 < v192; v74 += 1)
  {
    at(int32_t,*out,v74) = at(int32_t,v200,v74);
  }
  taskpool_shutdown();
  freeArray(v188);
  freeArray(e210);
  freeArray(v5);
  freeArray(v205);
  ivar_destroy(&e211);
  freeArray(v196);
  freeArray(v200);
  freeArray(v115);
  freeArray(v207);
  ivar_destroy(&e214);
}
