#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ksone

DP func(const DP x)
{
        return 1.0 - NR::erfcc(x/sqrt(2.0));
}

int main(void)
{
        const int NPTS=1000;
        const DP EPS=0.1;
        int i,j,idum=(-5);
        DP d,factr,prob,varnce;
        Vec_DP data(NPTS);

        cout << setw(19) << "variance ratio" << setw(17) << "k-s statistic";
        cout << setw(16) << "probability" << endl << endl;
        cout << fixed << setprecision(7);
        for (i=0;i<11;i++) {
          varnce=1.0+i*EPS;
          factr=sqrt(varnce);
          for (j=0;j<NPTS;j++)
            data[j]=factr*fabs(NR::gasdev(idum));
          NR::ksone(data,func,d,prob);
          cout << setw(16) << varnce << setw(17) << d;
          cout << setw(17) << prob << endl;
        }
        return 0;
}
