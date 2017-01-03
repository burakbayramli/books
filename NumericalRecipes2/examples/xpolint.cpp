#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine polint

int main(void)
{
        const DP PI=3.141592653589793238;
        int i,n,nfunc;
        DP dy,f,x,y;

        cout << "generation of interpolation tables" << endl;
        cout << " ... sin(x)    0<x<PI" << endl;
        cout << " ... exp(x)    0<x<1 " << endl;
        cout << "how many entries go in these tables?" << endl;
        cin >> n;
        cin.get();
        if (n < 1) return 1;
        Vec_DP xa(n);
        Vec_DP ya(n);
        cout << fixed << setprecision(6);
        for (nfunc=0;nfunc<2;nfunc++) {
          if (nfunc == 0) {
            cout << endl << "sine function from 0 to PI" << endl;
            for (i=0;i<n;i++) {
              xa[i]=(i+1)*PI/n;
              ya[i]=sin(xa[i]);
            }
          } else if (nfunc == 1) {
            cout << endl << "exponential function from 0 to 1" << endl;
            for (i=0;i<n;i++) {
              xa[i]=(i+1)*1.0/n;
              ya[i]=exp(xa[i]);
            }
          } else {
            break;
          }
          cout << setw(9) << "x" << setw(14) << "f(x)";
          cout << setw(17) << "interpolated" << setw(14) << "error" << endl;
          for (i=0;i<10;i++) {
            if (nfunc == 0) {
              x=(-0.05+(i+1)/10.0)*PI;
              f=sin(x);
            } else if (nfunc == 1) {
              x=(-0.05+(i+1)/10.0);
              f=exp(x);
            }
            NR::polint(xa,ya,x,y,dy);
            cout << setw(12) << x << setw(13) << f << setw(13) << y;
            cout << "     " << setw(12) << dy << endl;
          }
          cout << endl << "***********************************" << endl;
          cout << "press RETURN" << endl;
          cin.get();
        }
        return 0;
}
