// Graphical comparison of operation counts for various sorting methods
#include "memalloc.h"
#include "graphlib.h"

long ncomp, nsave;                               // no. of compares and saves
//===========================================================================
void BubbleSort(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by modified bubble sort
//---------------------------------------------------------------------------
{
   double xi;
   int i, ipass, swap;

   ipass = 0;                                      // initialize pass counter
   swap = 1;                            // initialize swap flag to enter loop
   while (swap) {                         // perform passes while swaps occur
      ipass ++;                                      // increase pass counter
      swap = 0;                                       // initialize swap flag
      for (i=1; i<=n-ipass; i++) {             // loop over unsorted sublists
         ncomp += 1;   //----------------------------------------------------
         if (x[i] > x[i+1]) {                            // compare neighbors
            nsave += 3;   //-------------------------------------------------
            xi = x[i]; x[i] = x[i+1]; x[i+1] = xi;          // swap neighbors
            swap = 1;                                        // set swap flag
         }
      }
   }
}

//===========================================================================
void InsertSort(double x[], int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[1..n] by direct insertion
//---------------------------------------------------------------------------
{
   double xpiv;
   int i, ipiv;

   for (ipiv=2; ipiv<=n; ipiv++) {                        // loop over pivots
      nsave += 1;   //-------------------------------------------------------
      xpiv = x[ipiv];                      // save pivot to free its location
      i = ipiv - 1;                             // initialize sublist counter
      ncomp += 1;   //-------------------------------------------------------
      while ((i > 0) && (x[i] > xpiv)) {         // scan to the left of pivot
         nsave += 1;   //----------------------------------------------------
         x[i+1] = x[i];                   // item > pivot: shift to the right
         i--;
      }
      nsave += 1;   //-------------------------------------------------------
      x[i+1] = xpiv;                 // insert pivot into last freed location
   }
}

//===========================================================================
void QuickSort(double x[], int l, int n)
//---------------------------------------------------------------------------
// Ascending sort of array x[l..n] by Quicksort
//---------------------------------------------------------------------------
{
   double t;
   int i, m;

   if (l >= n) return;
                                   // pivot = x[n]; create "<" and ">=" lists
   m = l;                                          // upper index in "<"-list
   for (i=l; i<=n-1; i++) {              // scan entire list, excepting pivot
      ncomp += 1;   //-------------------------------------------------------
      if (x[i] < x[n]) {                  // compare current value with pivot
         nsave += 3;   //----------------------------------------------------
         t = x[i]; x[i] = x[m]; x[m] = t;  // swap < value to end of "<"-list
         m++;                        // extend "<"-list: increase upper index
      }
   }
   nsave += 3;   //----------------------------------------------------------
   t = x[m]; x[m] = x[n]; x[n] = t;  // swap pivot between "<" and ">=" lists

   QuickSort(x,l,m-1);                                       // sort "<"-list

   QuickSort(x,m+1,n);                                      // sort ">="-list
}

int main(int argc, wchar_t** argv)
{
   double *x1, *x2, *y1, *y2;
   double *x, *x0;
   long i, ip, n, nf, np, ns;
   int nn[4], sty[4];                   // ending indexes and styles of plots
   const char* col[4];                                     // colors of plots

   nf = 100;                                                // scaling factor
   np = 100;                                     // number of plotting points
   ns = nf * np;                        // max. number of values to be sorted
   x = Vector(1,ns); x0 = Vector(1,ns);                // values to be sorted
   x1 = Vector(1,3*np); y1 = Vector(1,3*np);               // plotting points
   x2 = Vector(1,3*np); y2 = Vector(1,3*np);

   for (ip=1; ip<=np; ip++) {
      n = nf * ip;                           // number of values to be sorted
      printf("n = %i\n",n);

      for (i=1; i<=n; i++) x0[i] = rand();               // list to be sorted

      for (i=1; i<=n; i++) x[i] = x0[i];
      ncomp = 0; nsave = 0;
      BubbleSort(x,n);
      x1[ip] = log10(double(n)); y1[ip] = log10(double(ncomp));
      x2[ip] = log10(double(n)); y2[ip] = log10(double(ncomp+nsave));

      for (i=1; i<=n; i++) x[i] = x0[i];
      ncomp = 0; nsave = 0;
      InsertSort(x,n);
      x1[np+ip] = log10(double(n)); y1[np+ip] = log10(double(ncomp));
      x2[np+ip] = log10(double(n)); y2[np+ip] = log10(double(ncomp+nsave));

      for (i=1; i<=n; i++) x[i] = x0[i];
      ncomp = 0; nsave = 0;
      QuickSort(x,1,n);
      x1[2*np+ip] = log10(double(n)); y1[2*np+ip] = log10(double(ncomp));
      x2[2*np+ip] = log10(double(n)); y2[2*np+ip] = log10(double(ncomp+nsave));
   }

   PyGraph c(argc, argv);
   c.GraphInit(1200,600);

   nn[1] =   np; col[1] = "blue" ; sty[1] = 0;                 // Bubble sort
   nn[2] = 2*np; col[2] = "green"; sty[2] = 0;              // Insertion sort
   nn[3] = 3*np; col[3] = "red"  ; sty[3] = 0;                   // Quicksort
   c.MultiPlot(x1,y1,y1,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.10,0.45,0.15,0.85,"log(n)","log(Ncomp)",
               "Bubble, Insert, Quicksort - compares");

   c.MultiPlot(x2,y2,y2,nn,col,sty,3,10,0e0,0e0,0,0e0,0e0,0,
               0.60,0.95,0.15,0.85,"log(n)","log(Nop)",
               "Bubble, Insert, Quicksort - operations");

   c.MainLoop();
}
