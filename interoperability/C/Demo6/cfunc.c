#include <stdio.h>
void cfunc(double *p, int n) {

  for (int i = 0; i < n; i++)
    for (int j = 0; j<n; j++)
      p[i*n+j] *= 2.0;
  
  printf("Array from C is \n");
  for (int i = 0; i < n; i++) {
    for (int j = 0; j<n; j++)
      printf("%.6g \t",p[i*n+j]);
    printf("\n");
  }
}
