#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine anneal

int main(void)
{
        const int NCITY=10;
        int i,ii;
        int idum=(-111);
        Vec_INT iorder(NCITY);
        Vec_DP x(NCITY),y(NCITY);

        for (i=0;i<NCITY;i++) {
          x[i]=NR::ran3(idum);
          y[i]=NR::ran3(idum);
          iorder[i]=i;
        }
        NR::anneal(x,y,iorder);
        cout << endl << "*** System Frozen ***" << endl;
        cout << "Final path:" << endl;
        cout << setw(5) << "city" << setw(8) << "x" << setw(11) << "y" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<NCITY;i++) {
          ii=iorder[i];
          cout << setw(4) << ii << setw(11) << x[ii];
          cout << setw(11) << y[ii] << endl;
        }
        return 0;
}
