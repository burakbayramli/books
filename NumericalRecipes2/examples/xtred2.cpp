#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine tred2

int main(void)
{
        const int NP=10;
        const DP c_d[NP*NP]=
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
        int i,j,k,l,m;
        Vec_DP d(NP),e(NP);
        Mat_DP a(c_d,NP,NP),c(c_d,NP,NP),f(NP,NP);

        NR::tred2(a,d,e);
        cout << fixed << setprecision(2);
        cout << "diagonal elements" << endl;
        for (i=0;i<NP;i++) {
          cout << setw(12) << d[i];
          if ((i+1) % 5 == 0) cout << endl;
        }
        cout << "off-diagonal elements" << endl;
        for (i=1;i<NP;i++) {
          cout << setw(12) << e[i];
          if ((i+1) % 5 == 0) cout << endl;
        }
        // Check transformation matrix
        for (j=0;j<NP;j++) {
          for (k=0;k<NP;k++) {
            f[j][k]=0.0;
            for (l=0;l<NP;l++) {
              for (m=0;m<NP;m++)
                f[j][k] += a[l][j]*c[l][m]*a[m][k];
            }
          }
        }
        // How does it look?
        cout << endl << "tridiagonal matrix" << endl;
        for (i=0;i<NP;i++) {
          for (j=0;j<NP;j++) cout << setw(7) << f[i][j];
          cout << endl;
        }
        return 0;
}
