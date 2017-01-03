#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rkdumb

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
        dydx[0]= -y[1];
        dydx[1]=y[0]-(1.0/x)*y[1];
        dydx[2]=y[1]-(2.0/x)*y[2];
        dydx[3]=y[2]-(3.0/x)*y[3];
}

Vec_DP *xx_p;   // defining declaration
Mat_DP *y_p;

int main(void)
{
        const int NVAR=4,NSTEP=150;
        int j;
        DP x1=1.0,x2=20.0;
        Vec_DP vstart(NVAR);

        // Note: The arrays xx and y must have indices up to NSTEP
        xx_p=new Vec_DP(NSTEP+1);
        y_p=new Mat_DP(NVAR,NSTEP+1);
        Vec_DP &xx=*xx_p;
        Mat_DP &y=*y_p;
        vstart[0]=NR::bessj0(x1);
        vstart[1]=NR::bessj1(x1);
        vstart[2]=NR::bessj(2,x1);
        vstart[3]=NR::bessj(3,x1);
        NR::rkdumb(vstart,x1,x2,derivs);
        cout << setw(8) << "x" << setw(18) << "integrated";
        cout << setw(11) << "bessj3" << endl;
        cout << fixed << setprecision(6);
        for (j=10;j<=NSTEP;j+=10) {
          cout << setw(10) << xx[j] << setw(15) << y[3][j];
          cout << setw(13) << NR::bessj(3,xx[j]) << endl;
        }
        delete y_p;
        delete xx_p;
        return 0;
}
