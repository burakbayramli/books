#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine zbrak

DP fx(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        const int N=100,NBMAX=20;
        const DP X1=1.0,X2=50.0;
        int i,nb=NBMAX;
        Vec_DP xb1(NBMAX),xb2(NBMAX);

        NR::zbrak(fx,X1,X2,N,xb1,xb2,nb);
        cout << endl << "brackets for roots of bessj0:" << endl;
        cout << setw(19) << "lower" << setw(11) << "upper";
        cout << setw(16) << "f(lower)" << setw(11) << "f(upper)" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<nb;i++) {
          cout << "  root " << setw(2) << (i+1) << setw(11) << xb1[i];
          cout << setw(11) << xb2[i] << "   " << setw(11) << fx(xb1[i]);
          cout << setw(11) << fx(xb2[i]) << endl;
        }
        return 0;
}
