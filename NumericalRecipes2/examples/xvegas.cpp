#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine vegas

int idum;       // for ranno
int ndim;       // for fxn
DP xoff;

DP fxn(Vec_I_DP &pt, const DP wgt)
{
        int j;
        DP ans,sum;

        for (sum=0.0,j=0;j<ndim;j++) sum += (100.0*SQR(pt[j]-xoff));
        ans=(sum < 80.0 ? exp(-sum) : 0.0);
        ans *= pow(5.64189,DP(ndim));
        return ans;
}

int main(void)
{
        int init,itmax,j,ncall,nprn;
        DP avgi,chi2a,sd,xoff;

        cout << "IDUM = " << endl;
        cin >> idum;
        if (idum > 0) idum = -idum;
        cout << fixed << setprecision(6);
        for (;;) {
          cout << "Enter NDIM XOFF NCALL ITMAX NPRN (NDIM=0 to stop)" << endl;
          cin >> ndim >> xoff >> ncall >> itmax >> nprn;
          if (ndim <= 0) break;
          Vec_DP regn(2*ndim);
          avgi=sd=chi2a=0.0;
          for (j=0;j<ndim;j++) {
            regn[j]=0.0;
            regn[j+ndim]=1.0;
          }
          init = -1;
          NR::vegas(regn,fxn,init,ncall,itmax,nprn,avgi,sd,chi2a);
          cout << "Number of iterations performed: " << itmax << endl;
          cout << "Integral, Standard Dev., Chi-sq. = ";
          cout << setw(13) << avgi << setw(13) << sd;
          cout << setw(13) << chi2a << endl;
          init = 1;
          NR::vegas(regn,fxn,init,ncall,itmax,nprn,avgi,sd,chi2a);
          cout << "Additional iterations performed: " << itmax << endl;
          cout << "Integral, Standard Dev., Chi-sq. = ";
          cout << setw(13) << avgi << setw(13) << sd;
          cout << setw(13) << chi2a << endl << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
