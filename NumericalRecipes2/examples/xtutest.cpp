#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine tutest

int main(void)
{
        const int NPTS=5000,MPTS=1000,NSHFT=10;
        const DP EPS=0.02,VAR1=1.0,VAR2=4.0;
        int i,j,idum=(-51773);
        DP fctr1,fctr2,prob,t;
        Vec_DP data1(NPTS),data2(MPTS);

        // Generate two gaussian distributions of different variance
        fctr1=sqrt(VAR1);
        for (i=0;i<NPTS;i++) data1[i]=fctr1*NR::gasdev(idum);
        fctr2=sqrt(VAR2);
        for (i=0;i<MPTS;i++) data2[i]=NSHFT/2.0*EPS+fctr2*NR::gasdev(idum);
        cout << fixed << setprecision(2);
        cout << endl << "Distribution #1 : variance = ";
        cout << setw(6) << VAR1 << endl;
        cout << "Distribution #2 : variance = ";
        cout << setw(6) << VAR2 << endl << endl;
        cout << setw(7) << "shift" << setw(9) << "t";
        cout << setw(17) << "probability" << endl;
        for (i=0;i<NSHFT+1;i++) {
          NR::tutest(data1,data2,t,prob);
          cout << setw(6) << i*EPS << setw(11) << t;
          cout << setw(12) << prob << endl;
          for (j=0;j<NPTS;j++) data1[j] += EPS;
        }
        return 0;
}
