// Factorial with global variables
#include <stdio.h>

float f;                                                  // global variables
int n;

void Factorial()
{
   int i;

   f = 1e0;
   for (i=2; i<=n; i++) f *= i;
}

int main()
{
   printf("n = "); scanf("%d",&n);

   Factorial();

   printf("%d! = %f\n",n,f);
}
