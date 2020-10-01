#include "stdlib.h"
int *create_storage()
{
   /* Array of four integers. */
   return malloc(sizeof(int) * 4);
}

void destroy_storage(int *ptr)
{
   free(ptr);
}
