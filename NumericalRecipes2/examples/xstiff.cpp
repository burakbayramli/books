#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine stiff

int kmax,kount;        // defining declarations
DP dxsav;
Vec_DP *xp_p;
Mat_DP *yp_p;

int main(void)
{
        const int NP=3;
        DP eps,hstart,x1=0.0,x2=50.0;
        Vec_DP y(NP);
        int nbad,nok;

        for (;;) {
          cout << endl << "Enter eps,hstart (or eps=0.0 to end)" << endl;
          cin >> eps >> hstart;
          if (eps == 0.0) break;
          kmax=0;
          y[0]=y[1]=1.0;
          y[2]=0.0;
          NR::odeint(y,x1,x2,eps,hstart,0.0,nok,nbad,NR::derivs_s,NR::stiff);
          cout << fixed << setprecision(6);
          cout << endl << "successful steps:" << setw(14) << " ";
          cout << setw(4) << nok << endl;
          cout << "bad steps:" << setw(21) << " " << setw(4) << nbad << endl;
          cout << "y(end) = " << setw(13) << y[0] << setw(13) << y[1];
          cout << setw(13) << y[2] << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
