#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine mrqmin

int main(void)
{
        const int NPT=100,MA=6;
        const DP SPREAD=0.001;
        const DP a_d[MA]={5.0,2.0,3.0,2.0,5.0,3.0};
        const DP gues_d[MA]={4.5,2.2,2.8,2.5,4.9,2.8};
        int i,j,iter,itst,k,idum=(-911),mfit=MA;
        DP alamda,chisq,ochisq;
        Vec_BOOL ia(MA);
        Vec_DP x(NPT),y(NPT),sig(NPT);
        Vec_DP a(a_d,MA),gues(gues_d,MA);
        Mat_DP covar(MA,MA),alpha(MA,MA);

        // First try a sum of two Gaussians
        for (i=0;i<NPT;i++) {
          x[i]=0.1*(i+1);
          y[i]=0.0;
          for (j=0;j<MA;j+=3) {
            y[i] += a[j]*exp(-SQR((x[i]-a[j+1])/a[j+2]));
          }
          y[i] *= (1.0+SPREAD*NR::gasdev(idum));
          sig[i]=SPREAD*y[i];
        }
        for (i=0;i<mfit;i++) ia[i]=true;
        for (i=0;i<MA;i++) a[i]=gues[i];
        for (iter=0;iter<2;iter++) {
          alamda = -1;
          NR::mrqmin(x,y,sig,a,ia,covar,alpha,chisq,NR::fgauss,alamda);
          k=1;
          itst=0;
          for (;;) {
            cout << endl << "Iteration #" << setw(3) << k;
            cout << setw(18) << "chi-squared:" << setw(13) << chisq;
            cout << setw(11) << "alamda:" << setw(10) << alamda << endl;
            cout << setw(8) << "a[0]" << setw(9) << "a[1]";
            cout << setw(9) << "a[2]" << setw(9) << "a[3]";
            cout << setw(9) << "a[4]" << setw(9) << "a[5]" << endl;
            cout << fixed << setprecision(6);
            for (i=0;i<6;i++) cout << setw(9) << a[i];
            cout << endl;
            k++;
            ochisq=chisq;
            NR::mrqmin(x,y,sig,a,ia,covar,alpha,chisq,NR::fgauss,alamda);
            fabs(ochisq-chisq) < 0.1 ? itst++ : itst=0;
            if (itst < 4) continue;
            alamda=0.0;
            NR::mrqmin(x,y,sig,a,ia,covar,alpha,chisq,NR::fgauss,alamda);
            cout << endl << "Uncertainties:" << endl;
            for (i=0;i<6;i++) cout << setw(9) << sqrt(covar[i][i]);
            cout << endl;
            cout << endl << "Expected results:" << endl;
            cout << setw(9) << 5.0 << setw(9) << 2.0 << setw(9) << 3.0;
            cout << setw(9) << 2.0 << setw(9) << 5.0 << setw(9) << 3.0 << endl;
            break;
          }
          if (iter == 0) {
            cout << endl << "press return to continue with constraint" << endl;
            cin.get();
            cout << "holding a[1] and a[4] constant" << endl;
            for (j=0;j<MA;j++) a[j] += 0.1;
            a[1]=2.0;
            ia[1]=false;
            a[4]=5.0;
            ia[4]=false;
          }
        }
        return 0;
}
