#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rofunc

DP aa,abdevt;             // defining declaration
const Vec_DP *xt_p,*yt_p;

int main(void)
{
        const int NDATA=100;
        const DP SPREAD=0.05;
        int i;
        int idum=(-11);
        DP b,rf;

        Vec_DP x(NDATA),y(NDATA);
        for (i=0;i<NDATA;i++) {
          x[i]=0.1*(i+1);
          y[i] = -2.0*x[i]+1.0+SPREAD*NR::gasdev(idum);
        }
        xt_p=&x;
        yt_p=&y;
        cout << setw(9) << "b" << setw(10) << "a";
        cout << setw(13) << "ROFUNC" << setw(11) << "ABDEVT";
        cout << fixed << setprecision(2) << endl;
        for (i = -5;i<=5;i++) {
          b = -2.0+0.02*i;
          rf=NR::rofunc(b);
          cout << setw(10) << b << setw(10) << aa;
          cout << setw(12) << rf << setw(11) << abdevt << endl;
        }
        return 0;
}
