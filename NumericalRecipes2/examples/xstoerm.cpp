#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine stoerm

DP d2y1(DP x) {return sin(x)+x;}
DP d2y2(DP x) {return cos(x)+x*x-2;}

void derivs(const DP x, Vec_I_DP &y, Vec_O_DP &dydx)
{
        dydx[0]=x-y[0];
        dydx[1]=x*x-y[1];
}

int main(void)
{
        const int NVAR=4;
        const DP X1=0.0, HTOT=1.570796;
        int i;
        DP a1,a2,xf;
        Vec_DP y(NVAR),yout(NVAR),d2y(NVAR);

        y[0]=0.0;
        y[1]= -1.0;
        y[2]=2.0;
        y[3]=0.0;
        derivs(X1,y,d2y);
        xf=X1+HTOT;
        a1=d2y1(xf);
        a2=d2y2(xf);
        cout << "Stoermer's Rule:" << endl;
        cout << fixed << setprecision(6);
        for (i=5;i<=45;i+=10) {
          NR::stoerm(y,d2y,X1,HTOT,i,yout,derivs);
          cout << endl << "x=" << setw(6) << X1 << " to ";
          cout << setw(6) << (X1+HTOT) << " in ";
          cout << setw(2) << i <<" steps" << endl;
          cout << setw(14) << "integration" << setw(10) << "answers" << endl;
          cout << setw(12) << yout[0] << setw(13) << a1 << endl;
          cout << setw(12) << yout[1] << setw(13) << a2 << endl;
        }
        return 0;
}
