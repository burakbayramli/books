#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine irbit2

int main(void)
{
        const unsigned long twoton[16]={0x1L,0x2L,0x4L,0x8L,0x10L,
          0x20L,0x40L,0x80L,0x100L,0x200L,0x400L,0x800L,0x1000L,
          0x2000L,0x4000L,0x8000L};
        const int NBIN=15,NTRIES=10000;
        int i,ipts=0,j,n;
        unsigned long iseed=111;
        Vec_DP delay(NBIN);

        // Calculate distribution of runs of zeros
        for (i=0;i<NBIN;i++) delay[i]=0.0;
        for (i=0;i<NTRIES;i++) {
          if (NR::irbit2(iseed) == 1) {
            ++ipts;
            for (j=0;j<NBIN;j++) {
              if (NR::irbit2(iseed) == 1) {
                ++delay[j];
                break;
              }
            }
          }
        }
        cout << "distribution of runs of n zeros" << endl;
        cout << setw(6) << "n" << setw(23) << "probability";
        cout << setw(19) << "expected" << endl << endl;
        cout << fixed << setprecision(4);
        for (n=0;n<NBIN;n++) {
          cout << setw(6) << n << setw(20) << delay[n]/ipts;
          cout << setw(21) << 1.0/DP(twoton[n+1]) << endl;
        }
        return 0;
}
