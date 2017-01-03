#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chsone

int main(void)
{
        const int NBINS=10,NPTS=2000;
        int i,j,ibin,k,idum=(-15);
        DP chsq,df,prob,x;
        Vec_DP bins(NBINS),ebins(NBINS);

        for (j=0;j<NBINS;j++) bins[j]=0.0;
        for (k=0;k<NPTS;k++) {
          x=NR::expdev(idum);
          ibin=int(x*NBINS/3.0);
          if (ibin < NBINS) ++bins[ibin];
        }
        for (i=0;i<NBINS;i++)
          ebins[i]=3.0*NPTS/NBINS*exp(-3.0*(i+0.5)/NBINS);
        NR::chsone(bins,ebins,0,df,chsq,prob);
        cout << setw(15) << "expected" << setw(16) << "observed" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<NBINS;i++)
          cout << setw(14) << ebins[i] << setw(16) << bins[i] << endl;
        cout << endl << setw(19) << "chi-squared:";
        cout << setw(11) << chsq << endl;
        cout << setw(19) << "probability:" << setw(11) << prob << endl;
        return 0;
}
