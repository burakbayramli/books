#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine igray

int main(void)
{
        unsigned long jp,n,ng,nmax,nmin,nni,nxor;

        for (;;) {
          cout << endl << "input nmin,nmax: (nmin=nmax to end)" << endl;
          cin >> nmin >> nmax;
          if (nmin == nmax) break;
          jp=(nmax-nmin)/11;
          if (jp < 1) jp=1;
          cout << "n, Gray[n], Gray(Gray[n]), Gray[n] ^ Gray[n+1]" << endl;
          for (n=nmin;n<=nmax;n++) {
            ng=NR::igray(n,1);
            nni=NR::igray(ng,-1);
            if (nni != n)
              cout << "WRONG ! AT " << n << ", " << ng << ", " << nni << endl;
            if (((n-nmin) % jp) == 0) {
              nxor=ng ^ NR::igray(n+1,1);
              cout << n << " " << ng << " " << nni << " " << nxor << endl;
            }
          }
        }
        cout << "Normal completion" << endl;
        return 0;
}
