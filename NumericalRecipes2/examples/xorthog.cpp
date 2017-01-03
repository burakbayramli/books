#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine orthog

DP func(const DP x)
{
        return 1.0/SQR(1.0+x);
}

int main(void)
{
        int i,n;
        DP amu0,check,xx;

        // Test with w[x] = -log x
        for (;;) {
          cout << "Enter N (or 0 to stop): ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP alpha(2*n-1),beta(2*n-1);
          Vec_DP a(n),b(n),x(n),w(n),anu(2*n);
          alpha[0]=0.5;
          beta[0]=1.0;
          for (i=1;i<2*n-1;i++) {
            alpha[i]=0.5;
            beta[i]=1.0/(4.0*(4.0-1.0/(i*i)));
          }
          anu[0]=1.0;
          anu[1] = -0.25;
          for (i=1;i<2*n-1;i++)
            anu[i+1] = -anu[i]*(i+1)*i/(2.0*(i+2)*(2*i+1));
          NR::orthog(anu,alpha,beta,a,b);
          amu0=1.0;
          NR::gaucof(a,b,amu0,x,w);
          cout << "  #" << setw(13) << "x(i)" << setw(15) << "w(i)" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<n;i++) {
            cout << setw(3) << i << setw(15) << x[i];
            cout << setw(15) << w[i] << endl;
          }
          for (check=0.0,i=0;i<n;i++) check += w[i];
          cout << endl << "Check value: " << setw(12) << check;
          cout << " should be: " << setw(12) << amu0 << endl;
          // demonstrate the use of ORTHOG for an integral
          for (xx=0.0,i=0;i<n;i++) xx += w[i]*func(x[i]);
          cout << endl << "Integral from orthog: " << setw(12) << xx << endl;
          cout << "Actual value:         " << setw(12) << log(2.0);
          cout << endl << endl;
        }
        return 0;
}
