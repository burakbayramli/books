#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine trapzd

// Test function
DP func(const DP x)
{
        return (x*x)*(x*x-2.0)*sin(x);
}

// Integral of test function
DP fint(const DP x)
{
        return 4.0*x*(x*x-7.0)*sin(x)-(pow(x,4.0)-14.0*(x*x)+28.0)*cos(x);
}

int main(void)
{
        const int NMAX=14;
        const DP PIO2=1.570796326794896619;
        int i;
        DP a=0.0,b=PIO2,s;

        cout << endl << "Integral of func with 2^(n-1) points" << endl;
        cout << "Actual value of integral is ";
        cout << setw(10) << (fint(b)-fint(a)) << endl;
        cout << setw(6) << "n" << setw(24) << "approx. integral" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NMAX;i++) {
          s=NR::trapzd(func,a,b,i+1);
          cout << setw(6) << (i+1) << setw(20) << s << endl;
        }
        return 0;
}
