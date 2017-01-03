#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine lfit

void funcs(const DP x, Vec_O_DP &afunc)
{
        int i;

        int ma=afunc.size();
        afunc[0]=1.0;
        afunc[1]=x;
        for (i=2;i<ma;i++) afunc[i]=sin((i+1)*x);
}

int main(void)
{
        const int NPT=100,NTERM=5;
        const DP SPREAD=0.1;
        int i,j,idum=(-911);
        DP chisq;
        Vec_BOOL ia(NTERM);
        Vec_DP a(NTERM),x(NPT),y(NPT),sig(NPT);
        Mat_DP covar(NTERM,NTERM);

        for (i=0;i<NPT;i++) {
          x[i]=0.1*(i+1);
          funcs(x[i],a);
          y[i]=0.0;
          for (j=0;j<NTERM;j++) y[i] += (j+1)*a[j];
          y[i] += SPREAD*NR::gasdev(idum);
          sig[i]=SPREAD;
        }
        for (i=0;i<NTERM;i++) ia[i]=true;
        NR::lfit(x,y,sig,a,ia,covar,chisq,funcs);
        cout << endl << setw(11) << "parameter";
        cout << setw(22) << "uncertainty" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NTERM;i++) {
          cout << "  a[" << i << "] = " << setw(8) << a[i];
          cout << setw(13) << sqrt(covar[i][i]) << endl;
        }
        cout << "chi-squared = " << setw(12) << chisq << endl << endl;
        cout << "full covariance matrix" << endl;
        cout << scientific << setprecision(4);
        for (i=0;i<NTERM;i++) {
          for (j=0;j<NTERM;j++) cout << setw(15) << covar[i][j];
          cout << endl;
        }
        cout << endl << "press RETURN to continue..." << endl;
        cin.get();
        // Now check results of restricting fit parameters
        for (i=1;i<NTERM;i+=2) ia[i]=false;
        NR::lfit(x,y,sig,a,ia,covar,chisq,funcs);
        cout << endl << setw(11) << "parameter";
        cout << setw(22) << "uncertainty" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NTERM;i++) {
          cout << "  a[" << i << "] = " << setw(8) << a[i];
          cout << setw(13) << sqrt(covar[i][i]) << endl;
        }
        cout << "chi-squared = " << setw(12) << chisq << endl << endl;
        cout << "full covariance matrix" << endl;
        cout << scientific << setprecision(4);
        for (i=0;i<NTERM;i++) {
          for (j=0;j<NTERM;j++) cout << setw(15) << covar[i][j];
          cout << endl;
        }
        cout << endl;
        return 0;
}
