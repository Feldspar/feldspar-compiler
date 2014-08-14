#include <stdlib.h>
#include "MatMulC.h"

void MatMulC(int rows, int len, double *a, double *b, double *c)
{
  int i,j,k;

  for( i = 0; i < rows; i++ ) {
    for( j = 0; j < rows; j+=2 ) {
      double sum0 = 0.0;
      double sum1 = 0.0;
      for( k = 0; k < rows; k++ ) {
        sum0 += a[i*rows+k] * b[k*rows + j];
        sum1 += a[i*rows+k] * b[k*rows + j + 1];
      }
      c[i*rows + j] = sum0;
      c[i*rows + j+1] = sum1;
    }
  }
}
