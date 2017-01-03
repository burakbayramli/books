#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine powell

DP func(Vec_I_DP &x)
{
        return 0.5-NR::bessj0(SQR(x[0]-1.0)+SQR(x[1]-2.0)+SQR(x[2]-3.0));
}

int main(void)
{
        const int NDIM=3;
        const DP FTOL=1.0e-6;
        const DP p_d[NDIM]={1.5,1.5,2.5};
        int i,j,iter;
        DP fret;
        Vec_DP p(p_d,NDIM);
        Mat_DP xi(NDIM,NDIM);

        for (i=0;i<NDIM;i++)
          for (j=0;j<NDIM;j++)
            xi[i][j]=(i == j ? 1.0 : 0.0);
        NR::powell(p,xi,FTOL,iter,fret,func);
        cout << "Iterations: " << iter << endl << endl;;
        cout << "Minimum found at: " << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NDIM;i++) cout << setw(12) << p[i];
        cout << endl << endl << "Minimum function value = ";
        cout << setw(12) << fret << endl << endl;
        cout << "True minimum of function is at:" << endl;
        cout << setw(12) << 1.0 << setw(12) << 2.0 << setw(12) << 3.0 << endl;
        return 0;
}
