#include <stdio.h>

#define N 3

main()
{
  int i;
  int *stuff[2];

  stuff[0] = (int*)malloc( N*sizeof(int));
  stuff[1] = (int*)malloc( N*sizeof(int));

  for( i=0; i<N; i++)
  {
    stuff[0][i] = i+1;
    stuff[1][i] = 2*(i+1);
  }

  for( i=0; i<N; i++)
  {
    printf("stuff[0][%d] = %d\n", i, stuff[0][i]);
    printf("stuff[1][%d] = %d\n", i, stuff[1][i]);
    printf("\n");
  }

}
