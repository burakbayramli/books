#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine mmid

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
        dydx[0]= -y[1];
        dydx[1]=y[0]-(1.0/x)*y[1];
        dydx[2]=y[1]-(2.0/x)*y[2];
        dydx[3]=y[2]-(3.0/x)*y[3];
}

int main(void)
{
        const int NVAR=4;
        const DP X1=1.0,HTOT=0.5;
        int i;
        DP b1,b2,b3,b4,xf=X1+HTOT;
        Vec_DP y(NVAR),yout(NVAR),dydx(NVAR);

        y[0]=NR::bessj0(X1);
        y[1]=NR::bessj1(X1);
        y[2]=NR::bessj(2,X1);
        y[3]=NR::bessj(3,X1);
        derivs(X1,y,dydx);
        b1=NR::bessj0(xf);
        b2=NR::bessj1(xf);
        b3=NR::bessj(2,xf);
        b4=NR::bessj(3,xf);
        cout << "First four Bessel functions:" << endl;
        cout << fixed;
        for (i=5;i<=50;i+=5) {
          NR::mmid(y,dydx,X1,HTOT,i,yout,derivs);
          cout << setprecision(2);
          cout << endl << "x=" << setw(5) << X1 << " to ";
          cout << setw(5) << (X1+HTOT) << " in " << i << " steps" << endl;
          cout << setprecision(6);
          cout << setw(13) << "integration" << setw(10) << "bessj" << endl;
          cout << setw(12) << yout[0] << setw(12) << b1 << endl;
          cout << setw(12) << yout[1] << setw(12) << b2 << endl;
          cout << setw(12) << yout[2] << setw(12) << b3 << endl;
          cout << setw(12) << yout[3] << setw(12) << b4 << endl;
          cout << endl << "Press RETURN to continue..." << endl;
          cin.get();
        }
        return 0;
}
