#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ratint

DP f(const DP x, const DP eps)
{
        return x*exp(-x)/(SQR(x-1.0)+eps*eps);
}

int main(void)
{
        const int NPT=6;
        const DP EPS=1.0;
        int i;
        DP dyy,xx,yexp,yy;
        Vec_DP x(NPT),y(NPT);

        for (i=0;i<NPT;i++) {
          x[i]=(i+1)*2.0/NPT;
          y[i]=f(x[i],EPS);
        }
        cout << endl << "Diagonal rational function interpolation" << endl;
        cout << endl << setw(5) << "x" << setw(15) << "interp.";
        cout << setw(15) << "accuracy" << setw(13) << "actual" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<10;i++) {
          xx=0.2*(i+1);
          NR::ratint(x,y,xx,yy,dyy);
          yexp=f(xx,EPS);
          cout << setw(8) << xx << setw(13) << yy;
          cout << setw(14) << dyy << setw(14) << yexp << endl;
        }
        return 0;
}
