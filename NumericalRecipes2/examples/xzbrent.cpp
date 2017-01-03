#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine zbrent

DP fx(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        const int N=100,NBMAX=20;
        const DP X1=1.0,X2=50.0;
        int i,nb=NBMAX;
        DP tol,root;
        Vec_DP xb1(NBMAX),xb2(NBMAX);

        NR::zbrak(fx,X1,X2,N,xb1,xb2,nb);
        cout << endl << "Roots of bessj0:" << endl;
        cout << setw(20) << "x" << setw(16) << "f(x)" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nb;i++) {
          tol=(1.0e-6)*(xb1[i]+xb2[i])/2.0;
          root=NR::zbrent(fx,xb1[i],xb2[i],tol);
          cout << "root" << setw(4) << (i+1) << setw(15) << root;
          cout << setw(15) << fx(root) << endl;
        }
        return 0;
}
