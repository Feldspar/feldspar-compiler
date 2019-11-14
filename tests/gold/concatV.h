#ifndef TESTS_CONCATV_H
#define TESTS_CONCATV_H

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


void concatV(struct array * v1, struct array * * out);

#endif // TESTS_CONCATV_H
