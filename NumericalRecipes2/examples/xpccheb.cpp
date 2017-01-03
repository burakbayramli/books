#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine pccheb

int main(void)
{
        const int NCHECK=15,NFEW=13,NMANY=17;
        const DP PI=3.141592653589793238;
        int i,j;
        DP a=(-PI),b=PI,fac,f,sum,sume,py,py2;
        Vec_DP c(NMANY),cc(NFEW),d(NFEW),e(NMANY),ee(NFEW);

        // put power series of cos(PI*y) into e
        fac=1.0;
        e[0]=ee[0]=0.0;
        for (j=0;j<NMANY;j++) {
          i=j & 3;    // tricky way to perform j % 4
          if (i == 1 || i == 3) e[j]=0.0;
          else if (i == 0) e[j]=1.0/fac;
          else e[j] = -1.0/fac;
          fac *= (j+1);
        }
        for (j=0;j<NFEW;j++) ee[j]=e[j];
        NR::pcshft((-2.0-b-a)/(b-a),(2.0-b-a)/(b-a),e);
        // i.e., inverse of pcshft(a,b,...) which we do below
        NR::pccheb(e,c);
        cout << "Index, series, Chebyshev coefficients" << endl;
        cout << fixed << setprecision(6);
        for (j=0;j<NMANY;j+=2) {
          cout << setw(3) << j << setw(15) << e[j];
          cout << setw(15) << c[j] << endl;
        }
        for (j=0;j<NFEW;j++) cc[j]=c[j];
        NR::chebpc(cc,d);
        NR::pcshft(a,b,d);
        cout << "Index, new series, coefficient ratios" << endl;
        for (j=0;j<NFEW;j+=2) {
            cout << setw(3) << j << setw(15) << d[j];
            cout << setw(15) << d[j]/(ee[j]+1.0e-30) << endl;
        }
        cout << endl << "  " << "Point tested  " << " function value ";
        cout << " error power series " << " error Cheb. " << endl;
        for (i=0;i<=NCHECK;i++) {
          py=a+i*(b-a)/DP(NCHECK);
          py2=py*py;
          sum=sume=0.0;
          fac=1.0;
          for (j=0;j<NFEW;j+=2) {
            sum += fac*d[j];
            sume += fac*ee[j];
            fac *= py2;
          }
          f=cos(py);
          cout << setw(12) << py << setw(17) << f;
          cout << setw(17) << (sume-f) << setw(17) << (sum-f) << endl;
        }
        return 0;
}
