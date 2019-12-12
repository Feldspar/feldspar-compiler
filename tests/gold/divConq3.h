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


void task_core0(uint32_t v8, uint32_t v3, struct array * v1, struct array * v24);

void task0(void * params);

void divConq3(struct array * v1, struct array * * out);

#endif // TESTS_DIVCONQ3_H
