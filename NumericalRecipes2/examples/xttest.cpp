#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ttest

int main(void)
{
        const int NPTS=1024,MPTS=512,NSHFT=10;
        const DP EPS=0.02;
        int i,j,idum=(-5);
        DP prob,t;
        Vec_DP data1(NPTS),data2(MPTS);

        // Generate gaussian distributed data
        cout << setw(6) << "shift" << setw(9) << "t";
        cout << setw(17) << "probability" << endl << endl;
        for (i=0;i<NPTS;i++) data1[i]=NR::gasdev(idum);
        for (i=0;i<MPTS;i++) data2[i]=(NSHFT/2.0)*EPS+NR::gasdev(idum);
        cout << fixed << setprecision(2);
        for (i=0;i<NSHFT+1;i++) {
          NR::ttest(data1,data2,t,prob);
          cout << setw(6) << i*EPS << setw(11) << t;
          cout << setw(11) << prob << endl;
          for (j=0;j<NPTS;j++) data1[j] += EPS;
        }
        return 0;
}
