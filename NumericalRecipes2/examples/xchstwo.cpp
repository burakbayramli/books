#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine chstwo

int main(void)
{
        const int NBINS=10,NPTS=2000;
        int i,ibin,j,idum=(-17);
        DP chsq,df,prob,x;
        Vec_DP bins1(NBINS),bins2(NBINS);

        for (j=0;j<NBINS;j++) {
          bins1[j]=0.0;
          bins2[j]=0.0;
        }
        for (i=0;i<NPTS;i++) {
          x=NR::expdev(idum);
          ibin=int(x*NBINS/3.0);
          if (ibin < NBINS) ++bins1[ibin];
          x=NR::expdev(idum);
          ibin=int(x*NBINS/3.0);
          if (ibin < NBINS) ++bins2[ibin];
        }
        NR::chstwo(bins1,bins2,0,df,chsq,prob);
        cout << endl << setw(15) << "dataset 1";
        cout << setw(16) << "dataset 2" << endl;
        cout << fixed << setprecision(2);
        for (j=0;j<NBINS;j++)
          cout << setw(13) << bins1[j] << setw(16) << bins2[j] << endl;
        cout << endl << setw(18) << "chi-squared:";
        cout << setw(11) << chsq << endl;
        cout << setw(18) << "probability:" << setw(11) << prob << endl;
        return 0;
}
