#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine qroot

int main(void)
{
        const int N=6; // degree of polynomial
        const int NTRY=10;
        const DP EPS=1.0e-6,TINY=1.0e-5;
        const DP p_d[N+1]={10.0,-18.0,25.0,-24.0,16.0,-6.0,1.0};
        bool newroot;
        int i,j,nroot=0;
        Vec_DP p(p_d,N+1),b(NTRY),c(NTRY);

        cout << endl << "P(x)=x^6-6x^5+16x^4-24x^3+25x^2-18x+10" << endl;
        cout << "Quadratic factors x^2+bx+c" << endl << endl;
        cout << setw(6) << "factor" << setw(11) << "b";
        cout << setw(13) << "c" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NTRY;i++) {
          c[i]=0.5*(i+1);
          b[i] = -0.5*(i+1);
          NR::qroot(p,b[i],c[i],EPS);
          if (nroot == 0) {
            cout << setw(4) << nroot << setw(16) << b[i];
            cout << setw(13) << c[i] << endl;
            nroot=1;
          } else {
            newroot=true;
            for (j=0;j<nroot;j++)
              if ((fabs(b[i]-b[j]) < TINY) && (fabs(c[i]-c[j]) < TINY))
                newroot=false;
            if (newroot) {
              cout << setw(4) << nroot << setw(16) << b[i];
              cout << setw(13) << c[i] << endl;
              ++nroot;
            }
          }
        }
        return 0;
}
