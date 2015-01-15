#include "divConq3.h"


void task_core0(uint32_t v199, uint32_t v186, struct array * v0, struct array * v187, uint32_t v1)
{
  struct array * e1 = NULL;
  uint32_t len2;
  
  len2 = min(1024, (v199 - v186));
  e1 = initArray(e1, sizeof(int32_t), len2);
  for (uint32_t v2 = 0; v2 < len2; v2 += 1)
  {
    at(int32_t,e1,v2) = (at(int32_t,v0,(v2 + v186)) + 1);
  }
  ivar_put_array(at(struct ivar,v187,v1), e1);
}

void task0(void * params)
{
  run5(task_core0, uint32_t, uint32_t, struct array *, struct array *, uint32_t);
}

void task_core1(uint32_t v199, uint32_t v193, struct array * v0, struct array * v194, uint32_t v111)
{
  struct array * e8 = NULL;
  uint32_t len9;
  
  len9 = min(1024, (v199 - v193));
  e8 = initArray(e8, sizeof(int32_t), len9);
  for (uint32_t v112 = 0; v112 < len9; v112 += 1)
  {
    at(int32_t,e8,v112) = (at(int32_t,v0,(v112 + v193)) + 1);
  }
  ivar_put_array(at(struct ivar,v194,v111), e8);
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
  uint32_t len0;
  uint32_t v186;
  struct array * e3 = NULL;
  uint32_t len4;
  struct array * v5 = NULL;
  struct array * v201 = NULL;
  struct ivar e5;
  uint32_t v188;
  uint32_t v189;
  struct array * e6 = NULL;
  struct array * v198 = NULL;
  struct array * v194 = NULL;
  uint32_t len7;
  uint32_t v193;
  uint32_t len10;
  struct array * v115 = NULL;
  struct array * v202 = NULL;
  struct ivar e11;
  uint32_t v195;
  uint32_t v196;
  struct array * e12 = NULL;
  
  taskpool_init(4, 4, 4);
  v199 = getLength(v0);
  v200 = initArray(v200, sizeof(int32_t), 0);
  len0 = (v199 >> 10);
  v187 = initArray(v187, sizeof(struct ivar), len0);
  for (uint32_t v1 = 0; v1 < len0; v1 += 1)
  {
    v186 = (v1 << 10);
    ivar_init(&at(struct ivar,v187,v1));
    spawn5(task0, uint32_t, v199, uint32_t, v186, struct array *, v0, struct array *, v187, uint32_t, v1);
  }
  len4 = getLength(v187);
  e3 = initArray(e3, sizeof(int32_t), getLength(v200));
  copyArray(e3, v200);
  for (uint32_t v4 = 0; v4 < len4; v4 += 1)
  {
    e5 = at(struct ivar,v187,v4);
    v201 = ivar_get_array_nontask(v201, e5);
    v188 = getLength(e3);
    v189 = getLength(v201);
    v5 = setLength(v5, sizeof(int32_t), (v188 + v189));
    for (uint32_t v11 = 0; v11 < v188; v11 += 1)
    {
      at(int32_t,v5,v11) = at(int32_t,e3,v11);
    }
    for (uint32_t v22 = 0; v22 < v189; v22 += 1)
    {
      at(int32_t,v5,(v22 + v188)) = at(int32_t,v201,v22);
    }
    e6 = e3;
    e3 = v5;
    v5 = e6;
  }
  v191 = getLength(e3);
  len7 = (v199 >> 10);
  v194 = initArray(v194, sizeof(struct ivar), len7);
  for (uint32_t v111 = 0; v111 < len7; v111 += 1)
  {
    v193 = (v111 << 10);
    ivar_init(&at(struct ivar,v194,v111));
    spawn5(task1, uint32_t, v199, uint32_t, v193, struct array *, v0, struct array *, v194, uint32_t, v111);
  }
  len10 = getLength(v194);
  v198 = initArray(v198, sizeof(int32_t), getLength(v200));
  copyArray(v198, v200);
  for (uint32_t v114 = 0; v114 < len10; v114 += 1)
  {
    e11 = at(struct ivar,v194,v114);
    v202 = ivar_get_array_nontask(v202, e11);
    v195 = getLength(v198);
    v196 = getLength(v202);
    v115 = setLength(v115, sizeof(int32_t), (v195 + v196));
    for (uint32_t v121 = 0; v121 < v195; v121 += 1)
    {
      at(int32_t,v115,v121) = at(int32_t,v198,v121);
    }
    for (uint32_t v132 = 0; v132 < v196; v132 += 1)
    {
      at(int32_t,v115,(v132 + v195)) = at(int32_t,v202,v132);
    }
    e12 = v198;
    v198 = v115;
    v115 = e12;
  }
  *out = setLength(*out, sizeof(int32_t), v191);
  for (uint32_t v74 = 0; v74 < v191; v74 += 1)
  {
    at(int32_t,*out,v74) = at(int32_t,v198,v74);
  }
  taskpool_shutdown();
  freeArray(e3);
  freeArray(v5);
  ivar_destroy(&e5);
  freeArray(v115);
  ivar_destroy(&e11);
}
