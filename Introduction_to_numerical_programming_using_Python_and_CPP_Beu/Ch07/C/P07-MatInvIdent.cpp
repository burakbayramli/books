// Check matrix identity (A*B)^(-1) = B^(-1)*A^(-1)
#include <stdio.h>
#include "memalloc.h"
#include "linsys.h"
#include "matutil.h"

int main()
{
   double **a, **b, **c, **d, det, err;
   int i, j, n;

   n = 100;
   a = Matrix(1,n,1,n);
   b = Matrix(1,n,1,n);
   c = Matrix(1,n,1,n);
   d = Matrix(1,n,1,n);

   for (i=1; i<=n; i++)                 // generate matrices a and b randomly
      for (j=1; j<=n; j++) {
         a[i][j] = (double)rand()/RAND_MAX;
         b[i][j] = (double)rand()/RAND_MAX;
      }

   MatProd(a,b,c,n,n,n);                                         // c = a * b
   GaussJordan(c,d,n,0,det); // MatInv(c,n,det);            // c = (a b)^(-1)

   GaussJordan(a,d,n,0,det); // MatInv(a,n,det);               // a -> a^(-1)
   GaussJordan(b,d,n,0,det); // MatInv(b,n,det);               // b -> b^(-1)
   MatProd(b,a,d,n,n,n);                                 // d = b^(-1) a^(-1)
   
   MatDiff(c,d,d,n,n);                       // d = (a b)^(-1) - b^(-1)a^(-1)
   printf("(A B)^(-1) - B^(-1)A^(-1) (sample):\n\n");
   MatPrint(d,5,5);                          // print sample of "zero" matrix

   err = MatNorm(d,n,n);                                 // max(abs(d[i][j]))
   printf("\nMaximum error: %9.2e\n",err);
}
