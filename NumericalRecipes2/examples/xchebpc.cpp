#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chebpc

DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

int main(void)
{
        const int NVAL=40;
        const DP PIO2=1.570796326794896619;
        int i,j,mval;
        DP a=(-PIO2),b=PIO2,poly,x,y;
        Vec_DP c(NVAL);

        NR::chebft(a,b,c,func);
        for (;;) {
          cout << endl << "How many terms in Chebyshev evaluation?" << endl;
          cout << "Enter n between 2 and " << NVAL << ". (n<2 to end)." << endl;
          cin >> mval;
          cin.get();
          if ((mval < 2) || (mval > NVAL)) break;
          Vec_DP cc(mval),d(mval);
          for (i=0;i<mval;i++) cc[i]=c[i];
          NR::chebpc(cc,d);
          // Test polynomial
          cout << endl << setw(9) << "x" << setw(14) << "actual";
          cout << setw(14) << "polynomial" << endl;
          cout << fixed << setprecision(6);
          for (i = -8;i<=8;i++) {
            x=i*PIO2/10.0;
            y=(x-0.5*(b+a))/(0.5*(b-a));
            poly=d[mval-1];
            for (j=mval-2;j>=0;j--) poly=poly*y+d[j];
            cout << setw(12) << x << setw(12) << func(x);
            cout << setw(12) << poly << endl;
          }
        }
        return 0;
}
