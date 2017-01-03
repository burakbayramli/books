#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine hunt

int main(void)
{
        const int N=100;
        int i,j,ji;
        DP x;
        Vec_DP xx(N);

        // create array to be searched
        for (i=0;i<N;i++)
          xx[i]=exp((i+1)/20.0)-74.0;
        cout << endl << "  result of:   j= -1 indicates x too small" << endl;
        cout << setw(14) << "" << " j= 99 indicates x too large" << endl;
        cout << endl << setw(12) << "locate:" << setw(9) << "guess";
        cout << setw(5) << "j" << setw(12) << "xx(j)";
        cout << setw(14) << "xx(j+1)" << endl;
        // do test
        cout << fixed << setprecision(6);
        for (i=0;i<19;i++) {
          x = -100.0+10.0*(i+1);
          // trial parameter
          j=(ji=5*(i+1));
          // begin search
          NR::hunt(xx,x,j);
          if ((j < N-1) && (j > -1)) {
            cout << setw(12) << x << setw(7) << ji << setw(7) << j;
            cout << setw(13) << xx[j] << setw(13) << xx[j+1] << endl;
          } else if (j == N-1) {
            cout << setw(12) << x << setw(7) << ji << setw(7) << j;
            cout << setw(13) << xx[j] << "    upper lim" << endl;
          } else {
            cout << setw(12) << x << setw(7) << ji << setw(7) << j;
            cout << "    lower lim" << setw(13) << xx[j+1] << endl;
          }
        }
        return 0;
}
