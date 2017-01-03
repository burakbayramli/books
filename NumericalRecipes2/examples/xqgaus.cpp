#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine qgaus

DP func(const DP x)
{
        return x*exp(-x);
}

int main(void)
{
        const DP X1=0.0,X2=5.0;
        const int NVAL=10;
        int i;
        DP dx,ss,x;

        dx=(X2-X1)/NVAL;
        cout << endl << "0.0 to " << setw(11) << "qgaus";
        cout << setw(14) << "expected" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NVAL;i++) {
          x=X1+(i+1)*dx;
          ss=NR::qgaus(func,X1,x);
          cout << setw(5) << x << setw(13) << ss;
          cout << setw(13) << (-(1.0+x)*exp(-x)+(1.0+X1)*exp(-X1)) << endl;
        }
        return 0;
}
