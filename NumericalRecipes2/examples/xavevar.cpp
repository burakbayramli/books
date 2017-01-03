#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine avevar

int main(void)
{
        const int NPTS=10000;
        const DP EPS=0.1;
        int i,j,idum=(-5);
        DP ave,shift,vrnce;
        Vec_DP data(NPTS);

        // generate gaussian distributed data
        cout << endl << setw(9) << "shift" << setw(12) << "average";
        cout << setw(13) << "variance" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<11;i++) {
          shift=i*EPS;
          for (j=0;j<NPTS;j++)
            data[j]=shift+(i+1)*NR::gasdev(idum);
          NR::avevar(data,ave,vrnce);
          cout << setw(8) << shift << setw(12) << ave;
          cout << setw(13) << vrnce << endl;
        }
        return 0;
}
