#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine medfit

int main(void)
{
        const int NPT=100,NDATA=NPT;
        const DP SPREAD=0.1;
        bool mwt=true;
        int i,idum=(-1984);
        DP a,abdev,b,chi2,q,siga,sigb;
        Vec_DP x(NDATA),y(NDATA),sig(NDATA);

        for (i=0;i<NPT;i++) {
          x[i]=0.1*(i+1);
          y[i] = -2.0*x[i]+1.0+SPREAD*NR::gasdev(idum);
          sig[i]=SPREAD;
        }
        NR::fit(x,y,sig,mwt,a,b,siga,sigb,chi2,q);
        cout << endl << "According to routine FIT the result is:" << endl;
        cout << fixed << setprecision(4);
        cout << "   a =  " << setw(8) << a << "   uncertainty:  ";
        cout << setw(8) << siga << endl;
        cout << "   b =  " << setw(8) << b << "   uncertainty:  ";
        cout << setw(8) << sigb << endl;
        cout << "   chi-squared:  " << setw(8) << chi2;
        cout << "  for  " << NPT << "  points" << endl;
        cout << "   goodness-of-fit:  " << setw(8) << q << endl;
        cout << endl << "According to routine MEDFIT the result is:" << endl;
        NR::medfit(x,y,a,b,abdev);
        cout << "   a =  " << setw(8) << a << endl;
        cout << "   b =  " << setw(8) << b << endl;
        cout << "   absolute deviation (per data point): ";
        cout << setw(8) << abdev << endl;
        cout << "   (note: gaussian SPREAD is " << setw(8) << SPREAD << ")" <<endl;
        return 0;
}
