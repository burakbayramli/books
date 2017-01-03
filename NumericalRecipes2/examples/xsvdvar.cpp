#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine svdvar

int main(void)
{
        const int MA=3;
        const DP vtemp_d[MA*MA]=
          {1.0,1.0,1.0,
          2.0,2.0,2.0,
          3.0,3.0,3.0};
        const DP w_d[MA]={0.0,1.0,2.0};
        const DP tru[MA][MA]=
          {{1.25,2.5,3.75},
          {2.5,5.0,7.5},
          {3.75,7.5,11.25}};
        int i,j;
        Vec_DP w(w_d,MA);
        Mat_DP cvm(MA,MA),v(vtemp_d,MA,MA);

        cout << endl << "matrix v" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(12) << v[i][j];
          cout << endl;
        }
        cout << endl << "vector w" << endl;
        for (i=0;i<MA;i++) cout << setw(12) << w[i];
        cout << endl;
        NR::svdvar(v,w,cvm);
        cout << endl << "covariance matrix from svdvar" << endl;
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(12) << cvm[i][j];
          cout << endl;
        }
        cout << endl << "expected covariance matrix" << endl;
        for (i=0;i<MA;i++) {
          for (j=0;j<MA;j++) cout << setw(12) << tru[i][j];
          cout << endl;
        }
        return 0;
}
