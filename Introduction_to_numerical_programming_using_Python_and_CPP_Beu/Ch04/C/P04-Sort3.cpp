// Ascending sort of an array
#include "memalloc.h"
#include "sort.h"
#include "graphlib.h"

int main(int argc, wchar_t** argv)
{
   double *ix, *x, *x0;
   int *ind;
   int i, n;

   n = 50;                                   // number of values to be sorted
   x = Vector(1,n); x0 = Vector(1,n);          // array to be sorted and copy
   ix = Vector(1,n);                           // array of sequential indexes
   ind = IVector(1,n);                                    // array of indexes

   for (i=1; i<=n; i++) {
      x[i] = x0[i] = rand();                            // array to be sorted
      ix[i] = double(i);
   }

   PyGraph c(argc, argv);
   c.GraphInit(1200,600);

   c.Plot(ix,x,n,"blue",3,0.06,0.32,0.25,0.80,"i","x","Initial array");

   InsertSort(x,n);

   c.Plot(ix,x,n,"blue",3,0.39,0.65,0.25,0.80,"i","x","Sorted array");

   Index(x0,ind,n);                                       // sort by indexing
   for (i=1; i<=n; i++) x[i] = x0[ind[i]];

   c.Plot(ix,x,n,"blue",3,0.72,0.98,0.25,0.80,"i","x","Indexed array");

   c.MainLoop();
}
