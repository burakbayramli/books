#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine voltra

DP g(const int k, const DP t)
{
        return (k == 0 ? cosh(t)+t*sin(t) :
          2.0*sin(t)+t*(SQR(sin(t))+exp(t)));
}

DP ak(const int k, const int l, const DP t, const DP s)
{
        return ((k == 0) ?
          (l == 0 ? -exp(t-s) : -cos(t-s)) :
          (l == 0 ? -exp(t+s) : -t*cos(s)));
}

int main(void)
{
        const int N=10,M=2;
        const DP H=0.05;
        int nn;
        DP t0=0.0;
        Vec_DP t(N);
        Mat_DP f(M,N);

        NR::voltra(t0,H,t,f,g,ak);
        // exact soln is f[1]=exp(-t), f[2]=2sin(t)
        cout << "    abscissa     voltra       real";
        cout << "         voltra       real" << endl;
        cout << "                 answer1      answer1";
        cout << "      answer2      answer2" << endl << endl;
        cout << fixed << setprecision(6);
        for (nn=0;nn<N;nn++) {
          cout << setw(12) << t[nn] << setw(13) << f[0][nn];
          cout << setw(13) << exp(-t[nn]) << setw(13) << f[1][nn];
          cout << setw(13) << 2.0*sin(t[nn]) << endl;
        }
        return 0;
}
