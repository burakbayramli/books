#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine tqli

int main(void)
{
        const int NP=10;
        const DP TINY=1.0e-6;
        DP c_d[NP*NP]=
          {5.0, 4.3, 3.0, 2.0, 1.0, 0.0,-1.0,-2.0,-3.0,-4.0,
          4.3, 5.1, 4.0, 3.0, 2.0, 1.0, 0.0,-1.0,-2.0,-3.0,
          3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0,-1.0,-2.0,
          2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0,-1.0,
          1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0,
          0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0, 1.0,
          -1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0, 2.0,
          -2.0,-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 4.0, 3.0,
          -3.0,-2.0,-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 4.0,
          -4.0,-3.0,-2.0,-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0};
        int i,j,k;
        Vec_DP d(NP),e(NP),f(NP);
        Mat_DP a(c_d,NP,NP),c(c_d,NP,NP);

        NR::tred2(a,d,e);
        NR::tqli(d,e,a);
        cout << "Eigenvectors for a real symmetric matrix" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) {
            f[j]=0.0;
            for (k=0;k<NP;k++)
              f[j] += (c[j][k]*a[k][i]);
          }
          cout << "eigenvalue " << setw(3) << i << " = ";
          cout << setw(10) << d[i] << endl;
          cout << setw(10) << "vector" << setw(15) << "mtrx*vect.";
          cout << setw(10) << "ratio" << endl;
          for (j=0;j<NP;j++) {
            if (fabs(a[j][i]) < TINY) {
              cout << setw(12) << a[j][i] << setw(12) << f[j];
              cout << setw(12) << "div. by 0" << endl;
            } else {
              cout << setw(12) << a[j][i] << setw(12) << f[j];
              cout << setw(12) << f[j]/a[j][i] << endl;
            }
          }
          cout << "Press ENTER to continue..." << endl;
          cin.get();
        }
        return 0;
}
