#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ftest

int main(void)
{
        const int NVAL=11,NPTS=1000,MPTS=500;
        const DP EPS=0.01;
        int i,j,idum=(-13);
        DP f,factor,prob,vrnce;
        Vec_DP data1(NPTS),data2(MPTS),data3(MPTS);

        // Generate two gaussian distributions with different variances
        cout << fixed << setprecision(4);
        cout << endl << setw(16) << "Variance 1 = " << setw(5) << 1.0 << endl;
        cout << setw(13) << "Variance 2" << setw(12) << "Ratio";
        cout << setw(17) << "Probability" << endl;
        for (j=0;j<NPTS;j++) data1[j]=NR::gasdev(idum);
        for (j=0;j<MPTS;j++) data2[j]=NR::gasdev(idum);
        for (i=0;i<NVAL;i++) {
          vrnce=1.0+i*EPS;
          factor=sqrt(vrnce);
          for (j=0;j<MPTS;j++) data3[j]=factor*data2[j];
          NR::ftest(data1,data3,f,prob);
          cout << setw(11) << vrnce << setw(14) << f;
          cout << setw(14) << prob << endl;
        }
        return 0;
}
