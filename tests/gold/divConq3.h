#ifndef TESTS_DIVCONQ3_H
#define TESTS_DIVCONQ3_H

#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


void task_core0(uint32_t v199, uint32_t v186, struct array * v0, struct array * v187, uint32_t v1);

void task0(void * params);

void task_core1(uint32_t v199, uint32_t v193, struct array * v0, struct array * v194, uint32_t v111);

void task1(void * params);

void divConq3(struct array * v0, struct array * * out);

#endif // TESTS_DIVCONQ3_H
