#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine rkqs

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
        DP eps,hdid,hnext,htry,x=1.0;
        Vec_DP y(N),dydx(N),dysav(N),ysav(N),yscal(N);

        ysav[0]=NR::bessj0(x);
        ysav[1]=NR::bessj1(x);
        ysav[2]=NR::bessj(2,x);
        ysav[3]=NR::bessj(3,x);
        derivs(x,ysav,dysav);
        for (i=0;i<N;i++) yscal[i]=1.0;
        htry=0.6;
        cout << setw(10) << "eps" << setw(12) << "htry";
        cout << setw(13) << "hdid" << setw(14) << "hnext" << endl;
        cout << setprecision(6);
        for (i=0;i<15;i++) {
          eps=exp(-DP(i+1));
          x=1.0;
          for (j=0;j<N;j++) {
            y[j]=ysav[j];
            dydx[j]=dysav[j];
          }
          NR::rkqs(y,dydx,x,htry,eps,yscal,hdid,hnext,derivs);
          cout << scientific << setw(13) << eps;
          cout << fixed << setw(11) << htry;
          cout << setw(13) << hdid << setw(13) << hnext << endl;
        }
        return 0;
}
