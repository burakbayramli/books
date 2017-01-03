#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine dbrent

DP dfunc(const DP x)
{
        return -NR::bessj1(x);
}

DP func(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        const DP TOL=1.0e-6,EQL=1.0e-4;
        bool newroot;
        int i,j,nmin=0;
        DP ax,bx,cx,fa,fb,fc,xmin,dbr;
        Vec_DP amin(20);

        cout << endl << "Minima of the function bessj0" << endl;
        cout << setw(10) << "min. #" << setw(9) << "x";
        cout << setw(17) << "bessj0(x)" << setw(13) << "bessj1(x)";
        cout << setw(12) << "DBRENT" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<100;i++) {
          ax=DP(i);
          bx=DP(i+1);
          NR::mnbrak(ax,bx,cx,fa,fb,fc,func);
          dbr=NR::dbrent(ax,bx,cx,func,dfunc,TOL,xmin);
          if (nmin == 0) {
            amin[0]=xmin;
            nmin=1;
            cout << setw(7) << nmin << setw(16) << xmin;
            cout << setw(13) << func(xmin) << setw(13) << dfunc(xmin);
            cout << setw(13) << dbr << endl;
          } else {
            newroot=true;
            for (j=0;j<nmin;j++)
              if (fabs(xmin-amin[j]) <= EQL*xmin) newroot=false;
            if (newroot) {
              amin[nmin++]=xmin;
              cout << setw(7) << nmin << setw(16) << xmin;
              cout << setw(13) << func(xmin) << setw(13) << dfunc(xmin);
              cout << setw(13) << dbr << endl;
            }
          }
        }
        return 0;
}
