#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine gauleg

DP func(const DP x)
{
        return x*exp(-x);
}

int main(void)
{
        const int NPOINT=10;
        const DP X1=0.0,X2=1.0,X3=10.0;
        int i;
        DP xx=0.0;
        Vec_DP x(NPOINT),w(NPOINT);

        NR::gauleg(X1,X2,x,w);
        cout << endl << " #" << setw(11) << "x[i]";
        cout << setw(13) << "w[i]" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NPOINT;i++) {
          cout << setw(2) << i << setw(13) << x[i];
          cout << setw(13) << w[i] << endl;
        }
        // Demonstrate the use of gauleg for integration
        NR::gauleg(X1,X3,x,w);
        for (i=0;i<NPOINT;i++)
          xx += (w[i]*func(x[i]));
        cout << endl << "Integral from GAULEG: " << setw(12) << xx << endl;
        cout << "Actual value: " << setw(12) <<
          ((1.0+X1)*exp(-X1)-(1.0+X3)*exp(-X3)) << endl;
        return 0;
}
