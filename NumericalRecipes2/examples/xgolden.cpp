#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine golden

DP func(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        const DP TOL=1.0e-6,EQL=1.0e-3;
        bool newroot;
        int i,j,nmin=0;
        DP ax,bx,cx,fa,fb,fc,xmin;
        Vec_DP amin(20);

        cout << "Minima of the function bessj0" << endl;
        cout << setw(10) << "min. #" << setw(9) << "x";
        cout << setw(18) << "bessj0(x)" << setw(13) << "bessj1(x)" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<100;i++) {
          ax=DP(i);
          bx=DP(i+1);
          NR::mnbrak(ax,bx,cx,fa,fb,fc,func);
          NR::golden(ax,bx,cx,func,TOL,xmin);
          if (nmin == 0) {
            amin[nmin++]=xmin;
            cout << setw(7) << nmin << setw(16) << xmin;
            cout << setw(13) << NR::bessj0(xmin);
            cout << setw(13) << NR::bessj1(xmin);
            cout << endl;
          } else {
            newroot=true;
            for (j=0;j<nmin;j++)
              if (fabs(xmin-amin[j]) <= EQL*xmin) newroot=false;
            if (newroot) {
              amin[nmin++]=xmin;
              cout << setw(7) << nmin << setw(16) << xmin;
              cout << setw(13) << NR::bessj0(xmin);
              cout << setw(13) << NR::bessj1(xmin);
              cout << endl;
            }
          }
        }
        return 0;
}
