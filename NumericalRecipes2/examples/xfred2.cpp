#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fred2

DP g(const DP t)
{
        const DP PI=3.141592653589793238;

        return sqrt(t)-pow(PI/2.0,2.25)*pow(t,0.75)/2.25;
}

DP ak(const DP t, const DP s)
{
        return pow(t*s,0.75);
}


int main(void)
{
        const int N=8;
        const DP PI=3.141592653589793238;
        int i;
        DP a=0.0,b=PI/2.0;
        Vec_DP t(N),f(N),w(N);

        NR::fred2(a,b,t,f,w,g,ak);
        // Compare with exact solution
        cout << "  Abscissa, Calc soln, True soln" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++) {
          cout << setw(10) << t[i] << setw(11) << f[i];
          cout << setw(11) << sqrt(t[i]) << endl;
        }
        return 0;
}
