#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine gaucof

int main(void)
{
        // Test with Gauss-Hermite
        const DP SQRTPI=1.772453850905516027;
        int i,n;
        DP amu0,check;

        for (;;) {
          cout << "Enter N (or 0 to stop):  ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP a(n),b(n),x(n),w(n);
          for (i=0;i<n-1;i++) {
            a[i]=0.0;
            b[i+1]=(i+1)*0.5;
          }
          a[n-1]=0.0;
          // b[0] is arbitrary for call to tqli
          amu0=SQRTPI;
          NR::gaucof(a,b,amu0,x,w);
          cout << "  #" << setw(13) << "x(i)" << setw(15) << "w(i)" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<n;i++) {
            cout << setw(3) << i << setw(15) << x[i];
            cout << setw(15) << w[i] << endl;
          }
          for (check=0.0,i=0;i<n;i++) check += w[i];
          cout << endl << "Check value: " << setw(12) << check;
          cout << "  should be: " << setw(12) << SQRTPI << endl << endl;
        }
        return 0;
}
