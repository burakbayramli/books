#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine amoeba

DP func(Vec_I_DP &x)
{
        return 0.6-NR::bessj0(SQR(x[0]-0.5)+SQR(x[1]-0.6)+SQR(x[2]-0.7));
}

int main(void)
{
        const int MP=4,NP=3;
        const DP FTOL=1.0e-10;
        int i,nfunc,j;
        Vec_DP x(NP),y(MP);
        Mat_DP p(MP,NP);

        for (i=0;i<MP;i++) {
          for (j=0;j<NP;j++)
            x[j]=p[i][j]=(i == (j+1) ? 1.0 : 0.0);
          y[i]=func(x);
        }
        NR::amoeba(p,y,FTOL,func,nfunc);
        cout << endl << "Number of function evaluations: " << nfunc << endl;
        cout << "Vertices of final 3-d simplex and" << endl;
        cout << "function values at the vertices:" << endl << endl;
        cout << setw(3) << "i" << setw(10) << "x[i]";
        cout << setw(12) << "y[i]" << setw(12) << "z[i]";
        cout << setw(14) << "function" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<MP;i++) {
          cout << setw(3) << i;
          for (j=0;j<NP;j++) cout << setw(12) << p[i][j];
          cout << setw(12) << y[i] << endl;
        }
        cout << endl << "True minimum is at (0.5,0.6,0.7)" << endl;
        return 0;
}
