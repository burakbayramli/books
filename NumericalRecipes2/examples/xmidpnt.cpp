#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine midpnt

// Test function
DP func(const DP x)
{
        return 1.0/sqrt(x);
}

// Integral of test function
DP fint(const DP x)
{
        return 2.0*sqrt(x);
}

int main(void)
{
        const int NMAX=10;
        DP a=0.0,b=1.0,s;
        int i;

        cout << endl << "Integral of func computed with MIDPNT" << endl;
        cout << "Actual value of integral is ";
        cout << fixed << setprecision(6);
        cout << setw(9) << (fint(b)-fint(a)) << endl;
        cout << setw(6) << "n" << setw(29) << "Approx. integral" << endl;
        for (i=1;i<=NMAX;i++) {
          s=NR::midpnt(func,a,b,i);
          cout << setw(6) << i << setw(24) << s << endl;
        }
        return 0;
}
