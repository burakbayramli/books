#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine pade

DP fn(const DP x)
{
        return (x == 0.0 ? 1.0 : log(1.0+x)/x);
}

int main(void)
{
        int j,k,n;
        DP resid,b,d,fac,x;

        for (;;) {
          cout << "Enter n for PADE routine (or 0 to stop):  ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP c(2*n+1),cc(2*n+1);
          fac=1;
          for (j=0;j<2*n+1;j++) {
            c[j]=fac/DP(j+1);
            cc[j]=c[j];
            fac = -fac;
          }
          NR::pade(c,resid);
          cout << "Norm of residual vector= " << scientific;
          cout << setw(16) << resid << endl;
          cout << setw(14) << "point" << setw(19) << "func. value";
          cout << setw(16) << "pade series" << setw(16) << "power series" << endl;
          cout << fixed << setprecision(8);
          for (j=0;j<21;j++) {
            x=j*0.25;
            for (b=0.0,k=2*n;k>=0;k--) {
              b *= x;
              b += cc[k];
            }
            d=NR::ratval(x,c,n,n);
            cout << setw(16) << x << setw(16) << fn(x);
            cout << setw(16) << d << setw(16) << b << endl;
          }
          cout << endl;
        }
        return 0;
}
