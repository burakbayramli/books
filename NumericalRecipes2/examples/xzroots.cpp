#include <iostream>
#include <iomanip>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine zroots

int main(void)
{
        const int M=4, MP1=M+1;
        const complex<DP> real1(1.0,0.0),imag1(0.0,1.0);
        const complex<DP> a_d[MP1]=
          {2.0*imag1,0.0,-real1-2.0*imag1,0.0,real1};
        int i;
        bool polish;
        Vec_CPLX_DP a(a_d,MP1),roots(M);

        cout << endl << "Roots of the polynomial x^4-(1+2i)*x^2+2i" << endl;
        polish=false;
        NR::zroots(a,roots,polish);
        cout << endl << "Unpolished roots:" << endl;
        cout << setw(14) << "root #" << setw(14) << "root:" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<M;i++) {
          cout << setw(11) << noshowpos << i;
          cout << setw(25) << showpos << roots[i] << endl;
        }
        cout << endl << "Corrupted roots:" << endl;
        for (i=0;i<M;i++)
          roots[i]=(DP(1.0)+DP(0.01)*(i+1))*roots[i];
        cout << setw(14) << "root #" << setw(14) << "root:" << endl << endl;
        for (i=0;i<M;i++) {
          cout << setw(11) << noshowpos << i;
          cout << setw(25) << showpos << roots[i] << endl;
        }
        polish=true;
        NR::zroots(a,roots,polish);
        cout << endl << "Polished roots:" << endl;
        cout << setw(14) << "root #" << setw(14) << "root:" << endl << endl;
        for (i=0;i<M;i++) {
          cout << setw(11) << noshowpos << i;
          cout << setw(25) << showpos << roots[i] << endl;
        }
        return 0;
}
