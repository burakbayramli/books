#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chebft

DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

int main(void)
{
        const int NVAL=40;
        const DP PIO2=1.570796326794896619;
        DP a=(-PIO2),b=PIO2,dum,f,t0,t1,term,x,y;
        Vec_DP c(NVAL);
        int i,j,mval;

        NR::chebft(a,b,c,func);
        // test result
        for (;;) {
          cout << endl << "How many terms in Chebyshev evaluation?" << endl;
          cout << "Enter n between 2 and " << NVAL << ". (n<2 to end)." << endl;
          cin >> mval;
          cin.get();
          if ((mval < 2) || (mval > NVAL)) break;
          cout << endl << setw(9) << "x" << setw(14) << "actual";
          cout << setw(16) << "chebyshev fit" << endl;
          cout << fixed << setprecision(6);
          for (i = -8;i<=8;i++) {
            x=i*PIO2/10.0;
            y=(x-0.5*(b+a))/(0.5*(b-a));
            // Evaluate Chebyshev polynomial without CHEBEV
            t0=1.0;
            t1=y;
            f=c[1]*t1+c[0]*0.5;
            for (j=2;j<mval;j++) {
              dum=t1;
              t1=2.0*y*t1-t0;
              t0=dum;
              term=c[j]*t1;
              f += term;
            }
            cout << setw(12) << x << setw(12) << func(x);
            cout << setw(12) << f << endl;
          }
        }
        return 0;
}
