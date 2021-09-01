// Check matrix identity det(A*B) = det(A)*det(B)
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, **c, detA, detAB, detB;
   int i, j, n;

   n = 100;
   a = Matrix(1,n,1,n);
   b = Matrix(1,n,1,n);
   c = Matrix(1,n,1,n);

   for (i=1; i<=n; i++)                 // generate matrices a and b randomly
      for (j=1; j<=n; j++) {
         a[i][j] = (double)rand()/RAND_MAX;
         b[i][j] = (double)rand()/RAND_MAX;
      }

   MatProd(a,b,c,n,n,n);                                         // C = A * B
   Gauss(a,b,n,0,detA);                     // m=0: performs only elimination
   Gauss(b,b,n,0,detB);                     // and calculates the determinant
   Gauss(c,b,n,0,detAB);

   printf("det(A)  = %10.2e\n",detA);
   printf("det(B)  = %10.2e\n",detB);
   printf("det(AB) = %10.2e\n",detAB);
   printf("Error   = %10.2e\n",1e0 - detA*detB/detAB);
}
