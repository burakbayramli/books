#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine gauher

DP func(const DP x)
{
        return cos(x);
}

int main(void)
{
        const DP SQRTPI=1.772453850905516027;
        int i,n;
        DP check,xx;

        for (;;) {
          cout << "Enter N (or 0 to stop):  ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP x(n),w(n);
          NR::gauher(x,w);
          cout << "  #" << setw(13) << "x(i)" << setw(15) << "w(i)" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<n;i++) {
            cout << setw(3) << i << setw(15) << x[i];
            cout << setw(15) << w[i] << endl;
          }
          for (check=0.0,i=0;i<n;i++) check += w[i];
          cout << endl << "Check value: " << setw(12) << check;
          cout << "  should be: " << setw(12) << SQRTPI << endl;
          // demonstrate the use of GAUHER for an integral
          for (xx=0.0,i=0;i<n;i++) xx += w[i]*func(x[i]);
          cout << endl << "Integral from gauher: " << setw(12) << xx << endl;
          cout << "Actual value:         " << setw(12);
          cout << SQRTPI*exp(-0.25) << endl << endl;
        }
        return 0;
}
