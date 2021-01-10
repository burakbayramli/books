#include <stdio.h>
#include <math.h>

main()
{
  int i, j;

  printf("\n");
  for( i=-5; i<5; i++)
  {
    for( j=-5; j<-1; j++)
    {
      printf("i/j = %2d/%2d = %3d (%10.7f) (%10.7f)\n",
        i,
        j,
        i/j,
        (double)i/(double)j,
        floor((double)i/(double)j) );
      //printf("i/j = %2d/%2d = %3d (%10.7f)\n", i, j, i/j, (double)i/(double)j);
    }
    for( j=1; j<5; j++)
    {
      printf("i/j = %2d/%2d = %3d (%10.7f) (%10.7f)\n",
        i,
        j,
        i/j,
        (double)i/(double)j,
        floor((double)i/(double)j) );
      //printf("i/j = %2d/%2d = %3d (%10.7f)\n", i, j, i/j, (double)i/(double)j);
    }
  }
  printf("\n");
}
