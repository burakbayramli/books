#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine miser

int idum;      // for ranpt
int ndim;      // for func
DP xoff;

DP func(Vec_I_DP &pt)
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
        int j,n,nt,ntries;
        DP ave,dith,sumav,sumsd,var;

        cout << "IDUM = " << endl;
        cin >> idum;
        if (idum > 0) idum = -idum;
        for (;;) {
          cout << "Enter N NDIM XOFF DITH and NTRIES (N=0 to stop)" << endl;
          cin >> n >> ndim >> xoff >> dith >> ntries;
          if (n <= 0) break;
          Vec_DP regn(2*ndim);
          sumav=sumsd=0.0;
          for (nt=0;nt<ntries;nt++) {
            for (j=0;j<ndim;j++) {
              regn[j]=0.0;
              regn[ndim+j]=1.0;
            }
            NR::miser(func,regn,n,dith,ave,var);
            sumav += SQR(ave-1.0);
            sumsd += sqrt(fabs(var));
          }
          sumav=sqrt(sumav/ntries);
          sumsd /= ntries;
          cout << "Fractional error: actual,indicated = ";
          cout << fixed << setprecision(6);
          cout << setw(12) << sumav << setw(13) << sumsd << endl << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
