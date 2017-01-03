#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine gaujac

DP func(const DP ak, const DP x)
{
        return 1.0/sqrt(1.0-ak*ak*(1.0+x)/2.0);
}

int main(void)
{
        const DP PIBY2=1.570796326794896619;
        int i,n;
        DP ak,alf=(-0.5),bet=(-0.5),checkw,checkx,xx;

        for (;;) {
          cout << "Enter N (or 0 to stop):  ";
          cin >> n;
          cout << endl;
          if (n < 1) break;
          Vec_DP x(n),w(n);
          NR::gaujac(x,w,alf,bet);
          cout << "  #" << setw(13) << "x(i)" << setw(15) << "w(i)" << endl;
          cout << fixed << setprecision(6);
          for (i=0;i<n;i++) {
            cout << setw(3) << i << setw(15) << x[i];
            cout << setw(15) << w[i] << endl;
          }
          checkx=checkw=0.0;
          for (i=0;i<n;i++) {
            checkx += x[i];
            checkw += w[i];
          }
          cout << endl << "Check value: " << setw(16) << checkx;
          cout << "  should be: " << setw(16) << n*(bet-alf)/(alf+bet+2*n);
          cout << endl << "Check value: " << setw(16) << checkw;
          cout << "  should be: " << setw(16) <<
            exp(NR::gammln(1.0+alf)+NR::gammln(1.0+bet)-
              NR::gammln(2.0+alf+bet))*pow(2.0,alf+bet+1.0) << endl;
          // demonstrate the use of GAUJAC for an integral
          ak=0.5;
          for (xx=0.0,i=0;i<n;i++) xx += w[i]*func(ak,x[i]);
          cout << endl << "Integral from gaujac: ";
          cout << setw(12) << xx << endl;
          cout << "Actual value:         ";
          cout << setw(12) << 2.0*NR::ellf(PIBY2,ak)<< endl << endl;
        }
        return 0;
}
