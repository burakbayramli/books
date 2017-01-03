#include <iostream>
#include <iomanip>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine fixrts

int main(void)
{
        const int NPOLES=6;
        const DP d_d[NPOLES]={6.0,-15.0,20.0,-15.0,6.0,0.0};
        bool polish=true;
        int i;
        complex<DP> z1,z2;
        Vec_DP d(d_d,NPOLES);
        Vec_CPLX_DP zcoef(NPOLES+1),zeros(NPOLES);

        // finding roots of (z-1.0)^6=1.0
        // first write roots
        zcoef[NPOLES]=1.0;
        for (i=0;i<NPOLES;i++)
          zcoef[i] = complex<DP>(-d[NPOLES-1-i],0.0);
        NR::zroots(zcoef,zeros,polish);
        cout << "Roots of (z-1.0)^6 = 1.0" << endl << endl;
        cout << setw(24) << "Root" << setw(28) << "(z-1.0)^6" << endl << endl;
        cout << fixed << setprecision(6) << showpos;
        for (i=0;i<NPOLES;i++) {
          z1=zeros[i]-1.0;        // compute (z-1.0)^6=1.0
          z2=z1*z1*z1;
          z1=z2*z2;
          cout << setw(6) << i << setw(26) << zeros[i];
          cout << setw(25) << z1 << endl;
        }
        // now fix them to lie within unit circle
        NR::fixrts(d);
        // check results
        zcoef[NPOLES]=1.0;
        for (i=0;i<NPOLES;i++)
          zcoef[i] = complex<DP>(-d[NPOLES-1-i],0.0);
        NR::zroots(zcoef,zeros,polish);
        cout << endl << "Roots reflected in unit circle" << endl << endl;
        cout << setw(24) << "Root" << setw(28) << "(z-1.0)^6" << endl << endl;
        for (i=0;i<NPOLES;i++) {
          z1=zeros[i]-1.0;
          z2=z1*z1*z1;
          z1=z2*z2;
          cout << setw(6) << i << setw(26) << zeros[i];
          cout << setw(25) << z1 << endl;
        }
        return 0;
}
