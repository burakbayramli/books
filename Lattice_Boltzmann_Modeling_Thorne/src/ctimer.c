#include <stdio.h>
#include <time.h>

main()
{
  time_t t0, t1;
  clock_t t;
  clock_t ct0, ct1;

  time( &t0);
  ct0 = clock();
  printf("t0 = %d\n", t0);

  t = clock() + 3.1415926535*CLK_TCK;
  while( clock() < t){}

  time( &t1);
  ct1 = clock();
  printf("t1 = %d\n", t1);

  printf(" t1- t0 = %f seconds\n", difftime( t1, t0));
  printf("(ct1-ct0 = %f)/CLK_TCK = %f seconds\n",
    ((double)ct1 - (double)ct0),
    ((double)ct1 - (double)ct0)/(double)CLK_TCK);
}
