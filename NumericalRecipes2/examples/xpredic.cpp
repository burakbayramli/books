#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine predic

DP f(const int n, const int npts)
{
        const DP PI=3.141592653589793238;

        return exp(-1.0*n/npts)*sin(2.0*PI*n/50.0)
          +exp(-2.0*n/npts)*sin(2.2*PI*n/50.0);
}

int main(void)
{
        const int NPTS=500,NPOLES=10,NFUT=20;
        int i;
        DP dum;
        Vec_DP d(NPOLES),future(NFUT),data(NPTS);

        for (i=0;i<NPTS;i++) data[i]=f(i+1,NPTS);
        NR::memcof(data,dum,d);
        NR::fixrts(d);
        NR::predic(data,d,future);
        cout << setw(6) << "i" << setw(12) << "actual";
        cout << setw(13) << "PREDIC" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NFUT;i++) {
          cout << setw(6) << i << setw(13) << f(NPTS+i+1,NPTS);
          cout << setw(13) << future[i] << endl;
        }
        return 0;
}
