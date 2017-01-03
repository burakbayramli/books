#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine mgfas

int main(void)
{
        const int NSTEP=4,JMAX=33;
        int i,j,midl=JMAX/2;
        Mat_DP f(JMAX,JMAX),u(0.0,JMAX,JMAX);

        u[midl][midl]=2.0;
        NR::mgfas(u,2);
        cout << "MGFAS solution:" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<JMAX;i+=NSTEP) {
          for (j=0;j<JMAX;j+=NSTEP) cout << setw(8) << u[i][j];
          cout << endl;
        }
        cout << endl << " Test that solution satisfies difference equations:";
        cout << endl;
        for (i=NSTEP;i<JMAX-1;i+=NSTEP) {
          for (j=NSTEP;j<JMAX-1;j+=NSTEP)
            f[i][j]=u[i+1][j]+u[i-1][j]+u[i][j+1]+u[i][j-1]-
              4.0*u[i][j]+u[i][j]*u[i][j]/((JMAX-1)*(JMAX-1));
          cout << "       ";
          for (j=NSTEP;j<JMAX-1;j+=NSTEP)
            cout << setw(8) << f[i][j]*(JMAX-1)*(JMAX-1);
          cout << endl;
        }
        return 0;
}
