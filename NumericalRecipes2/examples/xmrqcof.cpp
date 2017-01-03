#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine mrqcof

int main(void)
{
        const int NPT=100,MA=6;
        const DP SPREAD=0.1;
        const DP a_d[MA]={5.0,2.0,3.0,2.0,5.0,3.0};
        const DP gues_d[MA]={4.9,2.1,2.9,2.1,4.9,3.1};
        int i,j,idum=(-911),mfit;
        DP chisq;
        Vec_BOOL ia(MA);
        Vec_DP beta(MA),x(NPT),y(NPT),sig(NPT);
        Vec_DP a(a_d,MA), gues(gues_d,MA);
        Mat_DP covar(MA,MA),alpha(MA,MA);

        // First try sum of two gaussians
        for (i=0;i<NPT;i++) {
          x[i]=0.1*(i+1);
          y[i]=0.0;
          y[i] += a[0]*exp(-SQR((x[i]-a[1])/a[2]));
          y[i] += a[3]*exp(-SQR((x[i]-a[4])/a[5]));
          y[i] *= (1.0+SPREAD*NR::gasdev(idum));
          sig[i]=SPREAD*y[i];
        }
        mfit=MA;
        for (i=0;i<mfit;i++) ia[i]=true;
        for (i=0;i<mfit;i++) a[i]=gues[i];
        NR::mrqcof(x,y,sig,a,ia,alpha,beta,chisq,NR::fgauss);
        cout << endl << "matrix alpha" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(12) << alpha[i][j];
          cout << endl;
        }
        cout << "vector beta" << endl;
        for (i=0;i<MA;i++) cout << setw(12) << beta[i];
        cout << endl << "chi-squared: " << setw(11) << chisq << endl << endl;
        // Next fix one line and improve the other
        mfit=3;
        for (i=0;i<mfit;i++) ia[i]=false;
        for (i=0;i<MA;i++) a[i]=gues[i];
        NR::mrqcof(x,y,sig,a,ia,alpha,beta,chisq,NR::fgauss);
        cout << "matrix alpha" << endl;
        for (i=0;i<mfit;i++) {
          for (j=0;j<mfit;j++) cout << setw(12) << alpha[i][j];
          cout << endl;
        }
        cout << "vector beta" << endl;
        for (i=0;i<mfit;i++) cout << setw(12) << beta[i];
        cout << endl << "chi-squared: " << setw(11) << chisq << endl << endl;
        return 0;
}
