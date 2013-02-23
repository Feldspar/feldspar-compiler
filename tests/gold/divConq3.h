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


void task_core3(struct array * v0, uint32_t v22, uint32_t v23, struct array * v24, uint32_t v12);

void task3(void * params);

void divConq3(struct array * v0, struct array * * out);

#endif // TESTS_DIVCONQ3_H
