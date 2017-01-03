#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine poldiv

int main(void)
{
        const int N=5,NV=3;
        const DP u_d[N+1]={-1.0,5.0,-10.0,10.0,-5.0,1.0};
        const DP v_d[NV+1]={1.0,3.0,3.0,1.0};
        int i;
        Vec_DP u(u_d,N+1),v(v_d,NV+1);
        Vec_DP q(N+1),r(N+1);

        NR::poldiv(u,v,q,r);
        cout << endl << setw(10) << "x^0" << setw(10) << "x^1";
        cout << setw(10) << "x^2" << setw(10) << "x^3";
        cout << setw(10) << "x^4" << setw(10) << "x^5" << endl << endl;
        cout << "quotient polynomial coefficients:" << endl;
        cout << fixed << setprecision(2);
        for (i=0;i<6;i++) cout << setw(10) << q[i];
        cout << endl << "expected quotient coefficients:" << endl;
        cout << setw(10) << 31.0 << setw(10) << -8.0 << setw(10) << 1.0;
        cout << setw(10) << 0.0 << setw(10) << 0.0 << setw(10) << 0.0;
        cout << endl;
        cout << "remainder polynomial coefficients:" << endl;
        for (i=0;i<4;i++) cout << setw(10) << r[i];
        cout << endl << "expected remainder coefficients:" << endl;
        cout << setw(10) << -32.0 << setw(10) << -80.0;
        cout << setw(10) << -80.0 << setw(10) << 0.0 << endl;
        return 0;
}
