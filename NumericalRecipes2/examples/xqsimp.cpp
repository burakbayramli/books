#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine qsimp

// Test function
DP func(const DP x)
{
        return x*x*(x*x-2.0)*sin(x);
}

// Integral of test function
DP fint(const DP x)
{
        return 4.0*x*(x*x-7.0)*sin(x)-(pow(x,4.0)-14.0*x*x+28.0)*cos(x);
}

int main(void)
{
        const DP PIO2=1.570796326794896619;
        DP a=0.0,b=PIO2,s;

        cout << "Integral of func computed with QSIMP" << endl << endl;
        cout << "Actual value of integral is "
          << setw(12) << (fint(b)-fint(a)) << endl;
        s=NR::qsimp(func,a,b);
        cout << "Result from routine QSIMP is " << setw(12) << s << endl;
        return 0;
}
