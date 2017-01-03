#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine kendl1

int main(void)
{
        const int NDAT=200;
        const string txt[5]={"RAN0","RAN1","RAN2","RAN3","RAN4"};
        int i,j;
        DP prob,tau,z;
        Vec_DP data1(NDAT),data2(NDAT);

        // Look for correlations in RAN0, RAN1, RAN2, RAN3 and RAN4
        cout << endl << "Pair correlations of RAN0 ... RAN4" << endl << endl;
        cout << setw(9) << "Program" << setw(18) << "Kendall tau";
        cout << setw(17) << "Deviation" << setw(19) << "Probability" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<5;i++) {
          int idum=(-1357);
          for (j=0;j<NDAT;j++) {
            if (i == 0) {
              data1[j]=NR::ran0(idum);
              data2[j]=NR::ran0(idum);
            } else if (i == 1) {
              data1[j]=NR::ran1(idum);
              data2[j]=NR::ran1(idum);
            } else if (i == 2) {
              data1[j]=NR::ran2(idum);
              data2[j]=NR::ran2(idum);
            } else if (i == 3) {
              data1[j]=NR::ran3(idum);
              data2[j]=NR::ran3(idum);
            } else if (i == 4) {
              data1[j]=NR::ran4(idum);
              data2[j]=NR::ran4(idum);
            }
          }
          NR::kendl1(data1,data2,tau,z,prob);
          cout << setw(8) << txt[i] << setw(18) << tau;
          cout << setw(18) << z << setw(18) << prob << endl;
        }
        return 0;
}
