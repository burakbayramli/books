#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine linmin


DP func(Vec_I_DP &x)
{
        int i;
        DP f=0.0;

        for (i=0;i<3;i++)
          f += (x[i]-1.0)*(x[i]-1.0);
        return f;
}

int main(void)
{
        const int NDIM=3;
        const DP PIO2=1.570796326794896619;
        int i,j;
        DP fret,sr2,x;
        Vec_DP p(NDIM),xi(NDIM);

        cout << endl << "Minimum of a 3-d quadratic centered" << endl;
        cout << "at (1.0,1.0,1.0). Minimum is found" << endl;
        cout << "along a series of radials." << endl << endl;
        cout << setw(9) << "x" << setw(12) << "y";
        cout << setw(12) << "z" << setw(14) << "minimum" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<11;i++) {
          x=PIO2*i/10.0;
          sr2=sqrt(2.0);
          xi[0]=sr2*cos(x);
          xi[1]=sr2*sin(x);
          xi[2]=1.0;
          p[0]=p[1]=p[2]=0.0;
          NR::linmin(p,xi,fret,func);
          for (j=0;j<3;j++) cout << setw(12) << p[j];
          cout << setw(12) << fret << endl;
        }
        return 0;
}
