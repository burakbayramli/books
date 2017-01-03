#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine spline

int main(void)
{
        const int N=20;
        const DP PI=3.141592653589793238;
        int i;
        DP yp1,ypn;
        Vec_DP x(N),y(N),y2(N);

        cout << endl << "second-derivatives for sin(x) from 0 to pi" << endl;
        // Generate array for interpolation
        for (i=0;i<20;i++) {
          x[i]=(i+1)*PI/N;
          y[i]=sin(x[i]);
        }
        // calculate 2nd derivative with spline
        yp1=cos(x[0]);
        ypn=cos(x[N-1]);
        NR::spline(x,y,yp1,ypn,y2);
        // test result
        cout << setw(23) << "spline" << setw(17) << "actual" << endl;
        cout << setw(7) << "angle" << setw(19) << "2nd deriv";
        cout << setw(17) << "2nd deriv" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++) {
          cout << setw(9) << x[i] << setw(17) << y2[i];
          cout << setw(17) << -sin(x[i]) << endl;
        }
        return 0;
}
