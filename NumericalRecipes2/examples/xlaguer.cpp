#include <iostream>
#include <iomanip>
#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine laguer

int main(void)
{
        const int M=4;         // degree of polynomial
        const int MP1=M+1;     // no. of coefficients
        const int NTRY=21;
        const DP EPS=1.0e-6;
        const complex<DP> real1(1.0,0.0),imag1(0.0,1.0);
        const complex<DP> a_d[MP1]=
          {2.0*imag1,0.0,-real1-2.0*imag1,0.0,real1};
        bool newroot;
        int i,its,j,n=0;
        complex<DP> x;
        Vec_CPLX_DP a(a_d,MP1),y(NTRY);

        cout << endl << "Roots of polynomial x^4-(1+2i)*x^2+2i" << endl;
        cout << endl << setw(22) << "Root";
        cout << setw(15) << "#iter" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NTRY;i++) {
          x=complex<DP>((i-10.0)/10.0,(i-10.0)/10.0);
          NR::laguer(a,x,its);
          if (n == 0) {
            n=1;
            y[0]=x;
            cout << setw(5) << n << setw(25) << x;
            cout << setw(6) << its << endl;
          } else {
            newroot=true;
            for (j=0;j<n;j++)
              if (abs(x-y[j]) <= EPS*abs(x)) newroot=false;
            if (newroot) {
              y[n++]=x;
              cout << setw(5) << n << setw(25) << x;
              cout << setw(6) << its << endl;
            }
          }
        }
        return 0;
}
