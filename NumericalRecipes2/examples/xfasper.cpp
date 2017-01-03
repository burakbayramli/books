#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fasper

int main(void)
{
        const int NP=90,MP=4096,NPR=11;
        const DP TWOPI=6.283185307179586476;
        int j=0,jmax,n,nout,idum=(-4);
        DP prob;
        Vec_DP x(NP),y(NP),px(MP),py(MP);

        for (n=0;n<NP+10;n++) {
          if (n != 2 && n != 3 && n != 5 && n != 20 &&
            n != 37 && n != 50 && n != 66 && n != 67 &&
            n != 82 && n != 92) {
            x[j]=n+1;
            y[j]=0.75*cos(0.6*x[j])+NR::gasdev(idum);
            ++j;
          }
        }
        cout << scientific << setprecision(6);
        NR::fasper(x,y,4.0,1.0,px,py,nout,jmax,prob);
        cout << "fasper results for test signal (cos(0.6x) + noise):" << endl;
        cout << "nout,jmax,prob = " << setw(5) << nout;
        cout << setw(5) << jmax << setw(15) << prob << endl << endl;
        for (n=MAX(0,jmax-NPR/2);n<MIN(nout,jmax+NPR/2+1);n++)
          cout << n << setw(15) << TWOPI*px[n] << setw(15) << py[n] << endl;
        return 0;
}
