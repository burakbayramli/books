#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine odeint

DP dxsav;  // defining declarations
int kmax,kount;
Vec_DP *xp_p;
Mat_DP *yp_p;

int nrhs;   // counts function evaluations

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
        nrhs++;
        dydx[0]= -y[1];
        dydx[1]=y[0]-(1.0/x)*y[1];
        dydx[2]=y[1]-(2.0/x)*y[2];
        dydx[3]=y[2]-(3.0/x)*y[3];
}

int main(void)
{
        const int N=4, KMAX=100;
        int i,nbad,nok;
        DP eps=1.0e-4,h1=0.1,hmin=0.0,x1=1.0,x2=10.0;
        Vec_DP ystart(N);

        ystart[0]=NR::bessj0(x1);
        ystart[1]=NR::bessj1(x1);
        ystart[2]=NR::bessj(2,x1);
        ystart[3]=NR::bessj(3,x1);
        nrhs=0;
        dxsav=(x2-x1)/20.0;
        kmax=KMAX;
        xp_p=new Vec_DP(KMAX);
        yp_p=new Mat_DP(N,KMAX);
        Vec_DP &xp=*xp_p;
        Mat_DP &yp=*yp_p;
        NR::odeint(ystart,x1,x2,eps,h1,hmin,nok,nbad,derivs,NR::rkqs);
        cout << endl << "successful steps:" << setw(14) << " ";
        cout << setw(4) << nok << endl;
        cout << "bad steps:" << setw(21) << " " << setw(4) << nbad << endl;
        cout << "function evaluations:" << setw(10) << " ";
        cout << setw(4) << nrhs << endl;
        cout << endl << "stored intermediate values:    ";
        cout << setw(4) << kount << endl;
        cout << endl << setw(8) << "x" << setw(18) << "integral";
        cout << setw(16) << "bessj(3,x)" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<kount;i++) {
          cout << setw(10) << xp[i] << setw(16) << yp[3][i];
          cout << setw(15) << NR::bessj(3,xp[i]) << endl;
        }
        delete yp_p;
        delete xp_p;
        return 0;
}
