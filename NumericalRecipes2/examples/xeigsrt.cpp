#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine eigsrt

int main(void)
{
        const int NP=10;
        const DP c_d[NP*NP]=
          {5.0,4.3,3.0,2.0,1.0,0.0,-1.0,-2.0,-3.0,-4.0,
          4.3,5.1,4.0,3.0,2.0,1.0,0.0,-1.0,-2.0,-3.0,
          3.0,4.0,5.0,4.0,3.0,2.0,1.0,0.0,-1.0,-2.0,
          2.0,3.0,4.0,5.0,4.0,3.0,2.0,1.0,0.0,-1.0,
          1.0,2.0,3.0,4.0,5.0,4.0,3.0,2.0,1.0,0.0,
          0.0,1.0,2.0,3.0,4.0,5.0,4.0,3.0,2.0,1.0,
          -1.0,0.0,1.0,2.0,3.0,4.0,5.0,4.0,3.0,2.0,
          -2.0,-1.0,0.0,1.0,2.0,3.0,4.0,5.0,4.0,3.0,
          -3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0,5.0,4.0,
          -4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0,5.0};
        int i,j,nrot;
        Vec_DP d(NP);
        Mat_DP v(NP,NP),e(c_d,NP,NP);

        cout << "****** Finding Eigenvectors ******" << endl;
        NR::jacobi(e,d,v,nrot);
        cout << "unsorted eigenvectors:" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<NP;i++) {
          cout << "eigenvalue " << i << " = " << setw(12) << d[i] << endl;
          cout << "eigenvector:" << endl;
          for (j=0;j<NP;j++) {
            cout << setw(12) << v[j][i];
            if ((j+1)%5 == 0) cout << endl;
          }
          cout << endl;
        }
        cout << endl << "****** Sorting Eigenvectors ******";
        cout << endl << endl;
        NR::eigsrt(d,v);
        cout << "sorted eigenvectors:" << endl;
        for (i=0;i<NP;i++) {
          cout << "eigenvalue " << i << " = " << setw(12) << d[i] << endl;
          cout << "eigenvector:" << endl;
          for (j=0;j<NP;j++) {
            cout << setw(12) << v[j][i];
            if ((j+1)%5 == 0) cout << endl;
          }
          cout << endl;
        }
        return 0;
}
