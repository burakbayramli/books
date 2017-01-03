#include <iostream>
#include <iomanip>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine hqr

int main(void)
{
        const int NP=5;
        const DP a_d[NP*NP]=
          {1.0,2.0,0.0,0.0,0.0,
          -2.0,3.0,0.0,0.0,0.0,
          3.0,4.0,50.0,0.0,0.0,
          -4.0,5.0,-60.0,7.0,0.0,
          -5.0,6.0,-70.0,8.0,-9.0};
        int i,j;
        Vec_CPLX_DP wri(NP);
        Mat_DP a(a_d,NP,NP);

        cout << "matrix:" << endl;
        cout << fixed << setprecision(6) << showpos;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(12) << a[i][j];
          cout << endl;
        }
        NR::balanc(a);
        NR::elmhes(a);
        NR::hqr(a,wri);
        cout << "eigenvalues:" << endl;
        cout << setw(11) << "real" << setw(12) << "imag." << endl;
        for (i=0;i<NP;i++)
          cout << setw(25) << wri[i] << endl;
        return 0;
}
