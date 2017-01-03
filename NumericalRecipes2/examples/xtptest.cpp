#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine tptest

int main(void)
{
        const int NPTS=500,NSHFT=10;
        const DP EPS=0.01,ANOISE=0.3;
        int i,j,idum=(-5);
        DP ave1,ave2,ave3,var1,var2,var3;
        DP offset,prob1,prob2,shift,t1,t2;
        Vec_DP data1(NPTS),data2(NPTS),data3(NPTS);

        cout << setw(29) << "Correlated:";
        cout << setw(32) << "Uncorrelated:" << endl << endl;
        cout << setw(6) << "Shift" << setw(12) << "t";
        cout << setw(18) << "Probability" << setw(12) << "t";
        cout << setw(18) << "Probability" << endl << endl;
        offset=(NSHFT/2)*EPS;
        for (j=0;j<NPTS;j++) {
          data1[j]=NR::gasdev(idum);
          data2[j]=data1[j]+ANOISE*NR::gasdev(idum);
          data3[j]=NR::gasdev(idum);
          data3[j] += ANOISE*NR::gasdev(idum);
        }
        NR::avevar(data1,ave1,var1);
        NR::avevar(data2,ave2,var2);
        NR::avevar(data3,ave3,var3);
        for (j=0;j<NPTS;j++) {
          data1[j] -= ave1-offset;
          data2[j] -= ave2;
          data3[j] -= ave3;
        }
        cout << fixed << setprecision(4);
        for (i=0;i<NSHFT;i++) {
          shift=(i+1)*EPS;
          for (j=0;j<NPTS;j++) {
            data2[j] += EPS;
            data3[j] += EPS;
          }
          NR::tptest(data1,data2,t1,prob1);
          NR::tptest(data1,data3,t2,prob2);
          cout << setw(6) << shift << setw(15) << t1;
          cout << setw(13) << prob1 << setw(17) << t2;
          cout << setw(13) << prob2 << endl;
        }
        return 0;
}
