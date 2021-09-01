// Factorial based on recursivity
#include <stdio.h>

//===========================================================================
double FactRec(int n)
//---------------------------------------------------------------------------
// Returns the factorial of n using recursivity
//---------------------------------------------------------------------------
{
   return ((n > 1) ? n * FactRec(n-1) : 1e0);
}

int main()
{
   int n;

   printf("n = "); scanf("%d",&n);

   printf("%d! = %f\n",n,FactRec(n));
}
