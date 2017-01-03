#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine svdfit

int main(void)
{
        const int NPT=100,NPOL=5;
        const DP SPREAD=0.002;
        int i,idum=(-911);
        DP chisq;
        Vec_DP x(NPT),y(NPT),sig(NPT),a(NPOL),w(NPOL);
        Mat_DP cvm(NPOL,NPOL),u(NPT,NPOL),v(NPOL,NPOL);

        cout << fixed << setprecision(6);
        for (i=0;i<NPT;i++) {
          x[i]=0.02*(i+1);
          y[i]=1.0+x[i]*(2.0+x[i]*(3.0+x[i]*(4.0+x[i]*5.0)));
          y[i] *= (1.0+SPREAD*NR::gasdev(idum));
          sig[i]=y[i]*SPREAD;
        }
        NR::svdfit(x,y,sig,a,u,v,w,chisq,NR::fpoly);
        NR::svdvar(v,w,cvm);
        cout << endl << "polynomial fit:" << endl << endl;
        for (i=0;i<NPOL;i++) {
          cout << setw(12) << a[i] << "  +-";
          cout << setw(11) << sqrt(cvm[i][i]) << endl;
        }
        cout << endl << "Chi-squared " << setw(12) << chisq << endl;
        NR::svdfit(x,y,sig,a,u,v,w,chisq,NR::fleg);
        NR::svdvar(v,w,cvm);
        cout << endl << "Legendre polynomial fit:" << endl << endl;
        for (i=0;i<NPOL;i++) {
          cout << setw(12) << a[i] << "  +-";
          cout << setw(11) << sqrt(cvm[i][i]) << endl;
        }
        cout << endl << "Chi-squared " << setw(12) << chisq << endl;
        return 0;
}
