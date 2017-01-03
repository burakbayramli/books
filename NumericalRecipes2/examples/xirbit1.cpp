#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine irbit1

int main(void)
{
        const int NBIN=15, NTRIES=10000;
        int i,ipts=0,j,n;
        unsigned long iseed=12345;
        DP twoinv;
        Vec_DP delay(NBIN);

        // Calculate distribution of runs of zeros
        for (i=0;i<NBIN;i++) delay[i]=0.0;
        cout << "distribution of runs of n zeros" << endl;
        cout << setw(6) << "n" << setw(23) << "probability";
        cout << setw(19) << "expected" << endl << endl;
        for (i=0;i<NTRIES;i++) {
          if (NR::irbit1(iseed) == 1) {
            ++ipts;
            for (j=0;j<NBIN;j++) {
              if (NR::irbit1(iseed) == 1) {
                ++delay[j];
                break;
              }
            }
          }
        }
        twoinv=0.5;
        cout << fixed << setprecision(4);
        for (n=0;n<NBIN;n++) {
          cout << setw(6) << n << setw(20) << delay[n]/ipts;
          cout << setw(21) << twoinv << endl;
          twoinv /= 2.0;
        }
        return 0;
}
