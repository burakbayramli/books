#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rk4

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
        dydx[0]= -y[1];
        dydx[1]=y[0]-(1.0/x)*y[1];
        dydx[2]=y[1]-(2.0/x)*y[2];
        dydx[3]=y[2]-(3.0/x)*y[3];
}

int main(void)
{
        const int N=4;
        int i,j;
        DP h,x=1.0;
        Vec_DP y(N),dydx(N),yout(N);

        y[0]=NR::bessj0(x);
        y[1]=NR::bessj1(x);
        y[2]=NR::bessj(2,x);
        y[3]=NR::bessj(3,x);
        derivs(x,y,dydx);
        cout << endl << setw(16) << "Bessel function:";
        cout << setw(6) << "j0" << setw(13) << "j1";
        cout << setw(13) << "j3" << setw(13) << "j4" << endl;
        cout << fixed;
        for (i=0;i<5;i++) {
          h=0.2*(i+1);
          NR::rk4(y,dydx,x,h,yout,derivs);
          cout << setprecision(2);
          cout << endl << "for a step size of: " << setw(6) << h << endl;
          cout << setprecision(6);
          cout << setw(12) << "rk4:";
          for (j=0;j<4;j++) cout << setw(13) << yout[j];
          cout << endl << setw(12) << "actual:" << setw(13) << NR::bessj0(x+h);
          cout << setw(13) << NR::bessj1(x+h) << setw(13) << NR::bessj(2,x+h);
          cout << setw(13) << NR::bessj(3,x+h) << endl;
        }
        return 0;
}
