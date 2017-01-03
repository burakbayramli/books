#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine splint

int main(void)
{
        const int NP=10;
        const DP PI=3.141592653589793238;
        int i,nfunc;
        DP f,x,y,yp1,ypn;
        Vec_DP xa(NP),ya(NP),y2(NP);

        for (nfunc=0;nfunc<2;nfunc++) {
          if (nfunc == 0) {
            cout << endl << "sine function from 0 to pi" << endl;
            for (i=0;i<NP;i++) {
              xa[i]=(i+1)*PI/NP;
              ya[i]=sin(xa[i]);
            }
            yp1=cos(xa[0]);
            ypn=cos(xa[NP-1]);
          } else if (nfunc == 1) {
            cout << endl << "exponential function from 0 to 1" << endl;
            for (i=0;i<NP;i++) {
              xa[i]=1.0*(i+1)/NP;
              ya[i]=exp(xa[i]);
            }
            yp1=exp(xa[0]);
            ypn=exp(xa[NP-1]);
          } else {
            break;
          }
          // Call spline to get second derivatives
          NR::spline(xa,ya,yp1,ypn,y2);
          // Call splint for interpolations
          cout << endl << setw(9) << "x" << setw(14) << "f(x)";
          cout << setw(18) << "interpolation" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<10;i++) {
            if (nfunc == 0) {
              x=(-0.05+(i+1)/10.0)*PI;
              f=sin(x);
            } else if (nfunc == 1) {
              x = -0.05+(i+1)/10.0;
              f=exp(x);
            }
            NR::splint(xa,ya,y2,x,y);
            cout << setw(12) << x << setw(13) << f;
            cout << setw(13) << y << endl;
          }
          cout << endl << "***********************************" << endl;
          cout << "Press RETURN" << endl;
          cin.get();
        }
        return 0;
}
