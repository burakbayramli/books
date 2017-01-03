#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine mnewt

void usrfun(Vec_I_DP &x, Vec_O_DP &fvec, Mat_O_DP &fjac)
{
        int i;

        int n=x.size();
        fjac[0][0] = -2.0*x[0];
        fjac[0][1] = -2.0*x[1];
        fjac[0][2] = -2.0*x[2];
        fjac[0][3] = 1.0;
        for (i=0;i<n;i++) fjac[1][i] = 2.0*x[i];
        fjac[2][0] = 1.0;
        fjac[2][1] = -1.0;
        fjac[2][2] = 0.0;
        fjac[2][3] = 0.0;
        fjac[3][0] = 0.0;
        fjac[3][1] = 1.0;
        fjac[3][2] = -1.0;
        fjac[3][3] = 0.0;
        fvec[0] = -SQR(x[0])-SQR(x[1])-SQR(x[2])+x[3];
        fvec[1] = SQR(x[0])+SQR(x[1])+SQR(x[2])+SQR(x[3])-1.0;
        fvec[2] = x[0]-x[1];
        fvec[3] = x[1]-x[2];
}

int main(void)
{
        const int NTRIAL=5,N=4;
        const DP TOLX=1.0e-6,TOLF=1.0e-6;
        int kk,k,i,j;
        DP xx;
        Vec_DP fvec(N),x(N);
        Mat_DP fjac(N,N);

        cout << fixed << setprecision(6);
        for (kk=0;kk<2;kk++) {
          for (k=0;k<3;k++) {
            xx=0.2001*k*(2*kk-1);
            cout << "Starting vector number " << (k+1) << endl << endl;
            for (i=0;i<4;i++) {
              x[i]=xx+0.2*(i+1);
              cout << setw(7) << "x[" << i << "] = ";
              cout << setw(12) << x[i] << endl;
            }
            cout << endl;
            for (j=0;j<NTRIAL;j++) {
              NR::mnewt(1,x,TOLX,TOLF);
              usrfun(x,fvec,fjac);
              cout << setw(5) << "i" << setw(12) << "x[i]";
              cout << setw(14) << "f" << endl;
              for (i=0;i<N;i++) {
                cout << setw(5) << i << setw(14) << x[i];
                cout << setw(15) << fvec[i] << endl;
              }
              cout << endl << "press RETURN to continue..." << endl;
              cin.get();
            }
          }
        }
        return 0;
}
