#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ratlsq

DP fn(const DP t)
{
        return atan(t);
}

int main(void)
{
        int j,kk,mm;
        DP a,b,dev,eee,fit,xs;

        cout << fixed << setprecision(6);
        for (;;) {
          cout << "enter a,b,mm,kk (all 0 to end): " << endl ;
          cin >> a >> b >> mm >> kk;
          if ((a==0.0) && (b==0.0) && (mm==0) && (kk==0)) break;
          Vec_DP cof(mm+kk+1);
          NR::ratlsq(fn,a,b,mm,kk,cof,dev);
          for (j=0;j<=mm+kk;j++)
            cout << "cof(" << j << ")= " << setw(12) << cof[j] << endl;
          cout << "maximum absolute deviation= " << setw(12) << dev << endl;
          cout << "     x            error          exact   " << endl;
          for (j=0;j<20;j++) {
            xs=a+(b-a)*j/49.0;
            fit=NR::ratval(xs,cof,mm,kk);
            eee=fn(xs);
            cout << setw(10) << xs << setw(15) << (fit-eee);
            cout << setw(15) << eee << endl;
          }
        }
        return 0;
}
