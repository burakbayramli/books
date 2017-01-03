#include <iostream>
#include <iomanip>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine zrhqr

int main(void)
{
        const int M=4;         // degree of polynomial
        const int MP1=M+1;     // no. of polynomial coefficients
        const DP a_d[MP1]={-1.0,0.0,0.0,0.0,1.0};
        int i;
        Vec_DP a(a_d,MP1);
        Vec_CPLX_DP rt(M);

        cout << endl << "Roots of polynomial x^4-1" << endl;
        cout << endl << "    #" << setw(17) << "Root" << endl << endl;
        NR::zrhqr(a,rt);
        cout << fixed << setprecision(6);
        for (i=0;i<M;i++) {
          cout << setw(5) << noshowpos << i;
          cout << setw(25) << showpos << rt[i] << endl;
        }
        return 0;
}
