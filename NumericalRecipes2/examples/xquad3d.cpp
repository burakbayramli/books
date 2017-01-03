#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine quad3d

static DP xmax;

DP func(const DP x, const DP y, const DP z)
{
        return x*x+y*y+z*z;
}

DP z1(const DP x, const DP y)
{
        return -sqrt(xmax*xmax-x*x-y*y);
}

DP z2(const DP x, const DP y)
{
        return sqrt(xmax*xmax-x*x-y*y);
}

DP yy1(const DP x)
{
        return -sqrt(xmax*xmax-x*x);
}

DP yy2(const DP x)
{
        return sqrt(xmax*xmax-x*x);
}

int main(void)
{
        const int NVAL=10;
        const DP PI=3.141592653589793238;
        int i;
        DP xmin,s;

        cout << "Integral of r^2 over a spherical volume" << endl << endl;
        cout << setw(11) << "radius" << setw(13) << "QUAD3D";
        cout << setw(12) << "Actual" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NVAL;i++) {
          xmax=0.1*(i+1);
          xmin = -xmax;
          s=NR::quad3d(func,xmin,xmax);
          cout << setw(12) << xmax << setw(13) << s;
          cout << setw(12) << 4.0*PI*pow(xmax,5.0)/5.0 << endl;
        }
        return 0;
}
