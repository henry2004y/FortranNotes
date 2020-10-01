#include "FcallC++_method1.h"
#include<stdio.h>

void f_(double *a, double *b, double *c) {
  *c = *a * *b;
  printf("Now in C\n");
}

double func_(double *a, double *b) {
  return (*a+*b);
}
