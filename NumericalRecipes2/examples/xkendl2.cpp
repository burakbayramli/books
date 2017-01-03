#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine kendl2

int main(void)
{
        const int NDAT=1000,IP=8,JP=8;
        const string txt[8]=
          {"000","001","010","011","100","101","110","111"};
        int ifunc,k,l,m,n,twoton;
        unsigned long iseed;
        DP prob,tau,z;
        Mat_DP tab(IP,JP);

        // Look for 'ones-after-zeros' in IRBIT1 and IRBIT2 sequences
        cout << "Are ones followed by zeros and vice-versa?" << endl;
        cout << fixed << setprecision(6);
        for (ifunc=0;ifunc<2;ifunc++) {
          iseed=2468;
          if (ifunc == 0)
            cout << "test of irbit1:" << endl;
          else
            cout << "test of irbit2:" << endl;
          for (k=0;k<IP;k++)
            for (l=0;l<JP;l++) tab[k][l]=0.0;
          for (m=1;m<=NDAT;m++) {
            k=0;
            twoton=1;
            for (n=0;n<3;n++) {
              if (ifunc == 0)
                k += (NR::irbit1(iseed)*twoton);
              else
                k += (NR::irbit2(iseed)*twoton);
              twoton *= 2;
            }
            l=0;
            twoton=1;
            for (n=0;n<3;n++) {
              if (ifunc == 0)
                l += (NR::irbit1(iseed)*twoton);
              else
                l += (NR::irbit2(iseed)*twoton);
              twoton *= 2;
            }
            ++tab[k][l];
          }
          NR::kendl2(tab,tau,z,prob);
          cout << "    ";
          for (n=0;n<8;n++) cout << setw(6) << txt[n];
          cout << endl;
          for (n=0;n<8;n++) {
            cout << setw(3) << txt[n];
            for (m=0;m<8;m++)
              cout << setw(6) << int(0.5+tab[n][m]);
            cout << endl;
          }
          cout << endl << setw(17) << "kendall tau";
          cout << setw(15) << "deviation" << setw(17) << "probability" << endl;
          cout << setw(15) << tau << setw(16) << z;
          cout << setw(16) << prob << endl << endl;
        }
        return 0;
}
