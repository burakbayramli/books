#include <string>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine beschb

int main(void)
{
        string txt;
        DP gam1,gam2,gampl,gammi,x,xgam1,xgam2,xgampl,xgammi;

        for (;;) {
          cout << "Enter x (magnitude < 0.5):" << endl;
          cin >> x;
          if ((x < -0.5) || (x > 0.5) || (x == 0.0)) break;
          NR::beschb(x,xgam1,xgam2,xgampl,xgammi);
          cout << setw(5) << "x" << endl;
          cout << setw(15) << "gam1" << setw(17) << "gam2";
          cout << setw(18) << "gampl" << setw(16) << "gammi" << endl;
          cout << setw(15) << "xgam1" << setw(17) << "xgam2";
          cout << setw(18) << "xgampl" << setw(16) << "xgammi" << endl;
          gampl=1.0/exp(NR::gammln(DP(1.0+x)));
          gammi=1.0/exp(NR::gammln(DP(1.0-x)));
          gam1=(gammi-gampl)/(2.0*x);
          gam2=(gammi+gampl)/2.0;
          cout << scientific << setprecision(2);
          cout << setw(5) << x << endl;
          cout << scientific << setprecision(6);
          cout << setw(16) << gam1 << setw(17) << gam2;
          cout << setw(17) << gampl << setw(17) << gammi << endl;
          cout << setw(16) << xgam1 << setw(17) << xgam2;
          cout << setw(17) << xgampl << setw(17) << xgammi << endl;
          cout << endl << endl;
        }
        return 0;
}
