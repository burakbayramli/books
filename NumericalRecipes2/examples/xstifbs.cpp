#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine stifbs

int kmax,kount;        // defining declarations
DP dxsav;
Vec_DP *xp_p;
Mat_DP *yp_p;

int main(void)
{
        int nbad,nok;
        DP eps,hstart,x1=0.0,x2=50.0;
        Vec_DP y(3);

        cout << fixed << setprecision(6);
        for (;;) {
          cout << endl << "Enter eps,hstart (or eps=0 to end)" << endl;
          cin >> eps >> hstart;
          if (eps == 0.0) break;
          kmax=0;
          y[0]=y[1]=1.0;
          y[2]=0.0;
          NR::odeint(y,x1,x2,eps,hstart,0.0,nok,nbad,NR::derivs_s,NR::stifbs);
          cout << fixed << setprecision(6);
          cout << endl << "successful steps:" << setw(14) << " ";
          cout << setw(4) << nok << endl;
          cout << "bad steps:" << setw(21) << " " << setw(4) << nbad << endl;
          cout << "y(end) = " << setw(12) << y[0] << setw(12) << y[1];
          cout << setw(12) << y[2] << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
