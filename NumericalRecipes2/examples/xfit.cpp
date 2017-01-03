#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine fit

int main(void)
{
        const int NPT=100;
        const DP SPREAD=0.5;
        bool mwt=false;
        int i,idum=(-117);
        DP a,b,chi2,q,siga,sigb;
        Vec_DP x(NPT),y(NPT),sig(NPT);

        for (i=0;i<NPT;i++) {
          x[i]=0.1*(i+1);
          y[i] = -2.0*x[i]+1.0+SPREAD*NR::gasdev(idum);
          sig[i]=SPREAD;
        }
        cout << fixed << setprecision(6);
        for (i=0;i<2;i++) {
          NR::fit(x,y,sig,mwt,a,b,siga,sigb,chi2,q);
          if (mwt)
            cout << endl << "Including standard deviations" << endl;
          else
            cout << endl << "Ignoring standard deviations" << endl;
          cout << setw(12) << "a  =  " << setw(10) << a;
          cout << setw(19) << "uncertainty:" << setw(10) << siga << endl;
          cout << setw(12) << "b  =  " << setw(10) << b;
          cout << setw(19) << "uncertainty:" << setw(10) << sigb << endl;
          cout << setw(19) << "chi-squared: " << setw(15) << chi2 << endl;
          cout << setw(23) << "goodness-of-fit: " << setw(11) << q << endl;
          mwt = !mwt;
        }
        return 0;
}
