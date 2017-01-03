#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine polcoe

int main(void)
{
        const int NP=5;
        const DP PI=3.14159265358979323846;
        int i,j,nfunc;
        DP f,sum,x;
        Vec_DP coeff(NP),xa(NP),ya(NP);

        cout << fixed << setprecision(6);
        for (nfunc=0;nfunc<2;nfunc++) {
          if (nfunc == 0) {
            cout << "sine function from 0 to PI" << endl << endl;
            for (i=0;i<NP;i++) {
              xa[i]=(i+1)*PI/NP;
              ya[i]=sin(xa[i]);
            }
          } else if (nfunc == 1) {
            cout << "exponential function from 0 to 1" << endl << endl;
            for (i=0;i<NP;i++) {
              xa[i]=1.0*(i+1)/NP;
              ya[i]=exp(xa[i]);
            }
          } else {
            break;
          }
          NR::polcoe(xa,ya,coeff);
          cout << "  coefficients" << endl;
          for (i=0;i<NP;i++) cout << setw(12) << coeff[i];
          cout << endl << endl << setw(9) << "x";
          cout << setw(14) << "f(x)" << setw(15) << "polynomial" << endl;
          for (i=0;i<10;i++) {
            if (nfunc == 0) {
              x=(-0.05+(i+1)/10.0)*PI;
              f=sin(x);
            } else if (nfunc == 1) {
              x = -0.05+(i+1)/10.0;
              f=exp(x);
            }
            sum=coeff[NP-1];
            for (j=NP-2;j>=0;j--) sum=coeff[j]+sum*x;
            cout << setw(12) << x << setw(13) << f;
            cout << setw(12) << sum << endl;
          }
          cout << endl << "************************************" << endl;
          cout << "press RETURN" << endl;
          cin.get();
        }
        return 0;
}
