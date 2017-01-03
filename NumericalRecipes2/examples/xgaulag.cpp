#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine gaulag

DP func(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        int i,n;
        DP alf=1.0,checkw,checkx,xx;

        for (;;) {
          cout << "Enter N (or 0 to end):  ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP x(n),w(n);
          NR::gaulag(x,w,alf);
          cout << "  #" << setw(13) << "x(i)" << setw(15) << "w(i)" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<n;i++) {
            cout << setw(3) << i << setw(15) << x[i];
            cout << setw(15) << w[i] << endl;
          }
          checkx=checkw=0.0;
          for (i=0;i<n;i++) {
            checkx += x[i];
            checkw += w[i];
          }
          cout << endl << "Check value: " << setw(15) << checkx;
          cout << "  should be:  " << setw(15) << n*(n+alf);
          cout << endl << "Check value: " << setw(15) << checkw;
          cout << "  should be:  " << setw(15) << exp(NR::gammln(1.0+alf));
          cout << endl;
          // demonstrate the use of GAULAG for an integral
          for (xx=0.0,i=0;i<n;i++) xx += w[i]*func(x[i]);
          cout << endl << "Integral from gaulag: " << setw(12) << xx << endl;
          cout << "Actual value:         ";
          cout << setw(12) << 1.0/(2.0*sqrt(2.0)) << endl << endl;
        }
        return 0;
}
