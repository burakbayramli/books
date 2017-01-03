#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine simpr

int main(void)
{
        const int NVAR=3;
        const DP X1=0.0,HTOT=50.0;
        int i;
        DP a1=0.5976,a2=1.4023,a3=0.0;
        Vec_DP y(NVAR),yout(NVAR),dfdx(NVAR),dydx(NVAR);
        Mat_DP dfdy(NVAR,NVAR);

        y[0]=y[1]=1.0;
        y[2]=0.0;
        NR::derivs_s(X1,y,dydx);
        NR::jacobn_s(X1,y,dfdx,dfdy);
        cout << "Test Problem:" << endl;
        cout << fixed;
        for (i=5;i<=50;i+=5) {
          NR::simpr(y,dydx,dfdx,dfdy,X1,HTOT,i,yout,NR::derivs_s);
          cout << setprecision(2);
          cout << endl << "x=" << setw(6) << X1 << " to ";
          cout << setw(6) << (X1+HTOT) << " in " << i;
          cout << " steps" << endl;
          cout << setprecision(6);
          cout << setw(14) << "integration" << setw(10) << "bessj" << endl;
          cout << setw(12) << yout[0] << setw(12) << a1 << endl;
          cout << setw(12) << yout[1] << setw(12) << a2 << endl;
          cout << setw(12) << yout[2] << setw(12) << a3 << endl;
        }
        return 0;
}
