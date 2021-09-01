// Calculates combinations using a factorial function
#include <stdio.h>

//===========================================================================
double Fact(int n)
//---------------------------------------------------------------------------
// Returns the factorial of n
//---------------------------------------------------------------------------
{
   double f;
   int i;

   f = 1e0;
   for (i=2; i<=n; i++) f *= i;
   return f;
}

int main()
{
   double C;
   int k, n;

   printf("n = "); scanf("%d",&n);
   printf("k = "); scanf("%d",&k);

   C = Fact(n)/(Fact(k)*Fact(n-k));

   printf("C(%d,%d) = %f\n",n,k,C);
}
