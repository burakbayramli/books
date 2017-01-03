#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chint

DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

DP fint(const DP x)
{
        return 4.0*x*(x*x-7.0)*sin(x)-(x*x*(x*x-14.0)+28.0)*cos(x);
}

int main(void)
{
        const int NVAL=40;
        const DP PIO2=1.570796326794896619;
        int i,mval;
        DP a=(-PIO2),b=PIO2,x;
        Vec_DP c(NVAL),cint(NVAL);

        NR::chebft(a,b,c,func);
        // test integral
        cout << fixed << setprecision(6);
        for (;;) {
          cout << endl << "How many terms in Chebyshev evaluation?" << endl;
          cout << "Enter n between 2 and " << NVAL << ". (n<2 to end)." << endl;
          cin >> mval;
          cin.get();
          if ((mval < 2) || (mval > NVAL)) break;
          NR::chint(a,b,c,cint,mval);
          cout << endl << setw(9) << "x" << setw(14) << "actual";
          cout << setw(16) << "Cheby. integ." << endl;
          for (i = -8;i<=8;i++) {
            x=i*PIO2/10.0;
            cout << setw(12) << x << setw(12) << (fint(x)-fint(-PIO2));
            cout << setw(12) << NR::chebev(a,b,cint,mval,x) << endl;
          }
        }
        return 0;
}
