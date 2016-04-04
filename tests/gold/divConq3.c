#include "divConq3.h"


void task_core0(uint32_t v199, uint32_t v186, struct array * v0, struct array * v187, uint32_t v1)
{
  struct array * e204 = NULL;
  uint32_t len205;
  
  len205 = min(1024, (v199 - v186));
  e204 = initArray(e204, sizeof(int32_t), len205);
  for (uint32_t v2 = 0; v2 < len205; v2 += 1)
  {
    at(int32_t,e204,v2) = (at(int32_t,v0,(v2 + v186)) + 1);
  }
  ivar_put_array(at(struct ivar,v187,v1), e204);
}

void task0(void * params)
{
  run5(task_core0, uint32_t, uint32_t, struct array *, struct array *, uint32_t);
}

void task_core1(uint32_t v199, uint32_t v193, struct array * v0, struct array * v194, uint32_t v111)
{
  struct array * e211 = NULL;
  uint32_t len212;
  
  len212 = min(1024, (v199 - v193));
  e211 = initArray(e211, sizeof(int32_t), len212);
  for (uint32_t v112 = 0; v112 < len212; v112 += 1)
  {
    at(int32_t,e211,v112) = (at(int32_t,v0,(v112 + v193)) + 1);
  }
  ivar_put_array(at(struct ivar,v194,v111), e211);
}

void task1(void * params)
{
  run5(task_core1, uint32_t, uint32_t, struct array *, struct array *, uint32_t);
}

void divConq3(struct array * v0, struct array * * out)
{
  uint32_t v199;
  struct array * v200 = NULL;
  uint32_t v191;
  struct array * v187 = NULL;
  uint32_t len203;
  uint32_t v186;
  struct array * e206 = NULL;
  uint32_t len207;
  struct array * v5 = NULL;
  struct array * v201 = NULL;
  struct ivar e208;
  uint32_t v188;
  uint32_t v189;
  struct array * e209 = NULL;
  struct array * v198 = NULL;
  struct array * v194 = NULL;
  uint32_t len210;
  uint32_t v193;
  uint32_t len213;
  struct array * v115 = NULL;
  struct array * v202 = NULL;
  struct ivar e214;
  uint32_t v195;
  uint32_t v196;
  struct array * e215 = NULL;
  
  taskpool_init(4, 4, 4);
  v199 = getLength(v0);
  v200 = initArray(v200, sizeof(int32_t), 0);
  len203 = (v199 >> 10);
  v187 = initArray(v187, sizeof(struct ivar), len203);
  for (uint32_t v1 = 0; v1 < len203; v1 += 1)
  {
    v186 = (v1 << 10);
    ivar_init(&at(struct ivar,v187,v1));
    spawn5(task0, uint32_t, v199, uint32_t, v186, struct array *, v0, struct array *, v187, uint32_t, v1);
  }
  len207 = getLength(v187);
  e206 = initArray(e206, sizeof(int32_t), 0);
  copyArray(e206, v200);
  for (uint32_t v4 = 0; v4 < len207; v4 += 1)
  {
    e208 = at(struct ivar,v187,v4);
    v201 = ivar_get_array_nontask(v201, e208);
    v188 = getLength(e206);
    v189 = getLength(v201);
    v5 = initArray(v5, sizeof(int32_t), (v188 + v189));
    for (uint32_t v11 = 0; v11 < v188; v11 += 1)
    {
      at(int32_t,v5,v11) = at(int32_t,e206,v11);
    }
    for (uint32_t v22 = 0; v22 < v189; v22 += 1)
    {
      at(int32_t,v5,(v22 + v188)) = at(int32_t,v201,v22);
    }
    e209 = e206;
    e206 = v5;
    v5 = e209;
  }
  v191 = getLength(e206);
  len210 = (v199 >> 10);
  v194 = initArray(v194, sizeof(struct ivar), len210);
  for (uint32_t v111 = 0; v111 < len210; v111 += 1)
  {
    v193 = (v111 << 10);
    ivar_init(&at(struct ivar,v194,v111));
    spawn5(task1, uint32_t, v199, uint32_t, v193, struct array *, v0, struct array *, v194, uint32_t, v111);
  }
  len213 = getLength(v194);
  v198 = initArray(v198, sizeof(int32_t), 0);
  copyArray(v198, v200);
  for (uint32_t v114 = 0; v114 < len213; v114 += 1)
  {
    e214 = at(struct ivar,v194,v114);
    v202 = ivar_get_array_nontask(v202, e214);
    v195 = getLength(v198);
    v196 = getLength(v202);
    v115 = initArray(v115, sizeof(int32_t), (v195 + v196));
    for (uint32_t v121 = 0; v121 < v195; v121 += 1)
    {
      at(int32_t,v115,v121) = at(int32_t,v198,v121);
    }
    for (uint32_t v132 = 0; v132 < v196; v132 += 1)
    {
      at(int32_t,v115,(v132 + v195)) = at(int32_t,v202,v132);
    }
    e215 = v198;
    v198 = v115;
    v115 = e215;
  }
  *out = initArray(*out, sizeof(int32_t), v191);
  for (uint32_t v74 = 0; v74 < v191; v74 += 1)
  {
    at(int32_t,*out,v74) = at(int32_t,v198,v74);
  }
  taskpool_shutdown();
  freeArray(v200);
  freeArray(v187);
  freeArray(e206);
  freeArray(v5);
  freeArray(v201);
  ivar_destroy(&e208);
  freeArray(v198);
  freeArray(v194);
  freeArray(v115);
  freeArray(v202);
  ivar_destroy(&e214);
}
