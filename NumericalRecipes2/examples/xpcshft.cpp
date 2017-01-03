#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine pcshft

DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

int main(void)
{
        const int NVAL=40;
        const DP PIO2=1.570796326794896619;
        int i,j,mval;
        DP a=(-PIO2),b=PIO2,poly,x;
        Vec_DP c(NVAL);

        NR::chebft(a,b,c,func);
        for (;;) {
          cout << endl << "How many terms in Chebyshev evaluation?" << endl;
          cout << "Enter n between 6 and " << NVAL << ". (n=0 to end)." << endl;
          cin >> mval;
          cin.get();
          if ((mval <= 0) || (mval > NVAL)) break;
          Vec_DP cc(mval),d(mval);
          for (i=0;i<mval;i++)
            cc[i]=c[i];
          NR::chebpc(cc,d);
          NR::pcshft(a,b,d);
          // Test shifted polynomial
          cout << endl << setw(9) << "x" << setw(15) << "actual";
          cout << setw(15) << "polynomial" << endl;
          cout << fixed << setprecision(6);
          for (i = -8;i<=8;i++) {
            x=i*PIO2/10.0;
            poly=d[mval-1];
            for (j=mval-2;j>=0;j--) poly=poly*x+d[j];
            cout << setw(12) << x << setw(13) << func(x);
            cout << setw(13) << poly << endl;
          }
        }
        return 0;
}
