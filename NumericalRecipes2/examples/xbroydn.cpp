#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine broydn

void funcv(Vec_I_DP &x, Vec_O_DP &f)
{
        f[0]=SQR(x[0])+SQR(x[1])-2.0;
        f[1]=exp(x[0]-1.0)+x[1]*SQR(x[1])-2.0;
}

int main(void)
{
        const int N=2;
        bool check;
        int i;
        Vec_DP x(N),f(N);

        x[0]=2.0;
        x[1]=0.5;
        NR::broydn(x,check,funcv);
        funcv(x,f);
        if (check) cout << "Convergence problems." << endl;
        cout << setw(7) << "Index" << setw(9) << "x";
        cout << setw(13) << "f" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<N;i++)
          cout << setw(6) << i << setw(13) << x[i] << setw(13) << f[i] << endl;
        return 0;
}
