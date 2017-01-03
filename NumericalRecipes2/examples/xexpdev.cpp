#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine expdev

int main(void)
{
        const int NPTS=10000;
        const DP EE=2.7182818284590;
        int i,j,total=0;
        int idum=(1);
        DP expect,xx,y;
        Vec_INT x(20);
        Vec_DP trig(21);

        for (i=0;i<20;i++) {
          trig[i]=i/20.0;
          x[i]=0;
        }
        trig[20]=1.0;
        for (i=0;i<NPTS;i++) {
          y=NR::expdev(idum);
          for (j=0;j<20;j++)
            if ((y < trig[j+1]) && (y > trig[j])) ++x[j];
        }
        for (i=0;i<20;i++) total += x[i];
        cout << endl << "exponential distribution with ";
        cout << NPTS << " points" << endl;
        cout << "    interval      observed    expected" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<20;i++) {
          xx=DP(x[i])/total;
          expect=exp(-(trig[i]+trig[i+1])/2.0);
          expect *= (0.05*EE/(EE-1));
          cout << setprecision(2);
          cout << setw(8) << trig[i] << setw(6) << trig[i+1];
          cout << setprecision(6);
          cout << setw(12) << xx << setw(12) << expect << endl;
        }
        return 0;
}
