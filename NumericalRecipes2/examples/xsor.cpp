#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine sor

int main(void)
{
        const int NSTEP=4,JMAX=33;
        const DP PI=3.141592653589793238;
        int i,j,midl;
        DP rjac;
        Mat_DP a(1.0,JMAX,JMAX),b(1.0,JMAX,JMAX),c(1.0,JMAX,JMAX);
        Mat_DP d(1.0,JMAX,JMAX),e(-4.0,JMAX,JMAX),f(0.0,JMAX,JMAX);
        Mat_DP u(0.0,JMAX,JMAX);

        midl=JMAX/2;
        f[midl][midl]=2.0/((JMAX-1)*(JMAX-1));
        rjac=cos(PI/JMAX);
        NR::sor(a,b,c,d,e,f,u,rjac);
        cout << "SOR solution:" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<JMAX;i+=NSTEP) {
          for (j=0;j<JMAX;j+=NSTEP) cout << setw(8) << u[i][j];
          cout << endl;
        }
        cout << endl << " Test that solution satisfies difference equations:";
        cout << endl;
        for (i=NSTEP;i<JMAX-1;i+=NSTEP) {
          for (j=NSTEP;j<JMAX-1;j+=NSTEP)
            f[i][j]=u[i+1][j]+u[i-1][j]+u[i][j+1]
              +u[i][j-1]-4.0*u[i][j];
          cout << "       ";
          for (j=NSTEP;j<JMAX-1;j+=NSTEP) cout << setw(8) << f[i][j];
          cout << endl;
        }
        return 0;
}
