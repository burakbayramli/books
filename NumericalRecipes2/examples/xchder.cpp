#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chder

DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

DP fder(const DP x)
{
        return 4.0*x*(x*x-1.0)*sin(x)+x*x*(x*x-2.0)*cos(x);
}

int main(void)
{
        const int NVAL=40;
        const DP PIO2=1.570796326794896619;
        int i,mval;
        DP a=(-PIO2),b=PIO2,x;
        Vec_DP c(NVAL),cder(NVAL);

        NR::chebft(a,b,c,func);
        // Test derivative
        cout << fixed << setprecision(6);
        for (;;) {
          cout << endl << "How many terms in Chebyshev evaluation?" << endl;
          cout << "Enter n between 2 and " << NVAL << ". (n<2 to end)." << endl;
          cin >> mval;
          cin.get();
          if ((mval < 2) || (mval > NVAL)) break;
          NR::chder(a,b,c,cder,mval);
          cout << endl << setw(9) << "x" << setw(15) << "actual";
          cout << setw(17) << "Cheby. deriv." << endl;
          for (i = -8;i<=8;i++) {
            x=i*PIO2/10.0;
            cout << setw(12) << x << setw(13) << fder(x);
            cout << setw(13) << NR::chebev(a,b,cder,mval,x) << endl;
          }
        }
        return 0;
}
