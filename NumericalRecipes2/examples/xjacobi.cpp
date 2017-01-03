#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine jacobi

int main(void)
{
        const int NMAT=3;
        DP a_d[3*3]=
          {1.0,2.0,3.0,
          2.0,2.0,3.0,
          3.0,3.0,3.0};
        DP b_d[5*5]=
          {-2.0,-1.0,0.0,1.0,2.0,
          -1.0,-1.0,0.0,1.0,2.0,
          0.0,0.0,0.0,1.0,2.0,
          1.0,1.0,1.0,1.0,2.0,
          2.0,2.0,2.0,2.0,2.0};
        DP c_d[10*10]=
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
        int i,j,k,l,kk,ll,nrot;
        int num[3]={3,5,10};
        Mat_DP a(a_d,3,3),b(b_d,5,5),c(c_d,10,10);
        Mat_DP e[3]={a,b,c};

        cout << fixed << setprecision(6);
        for (i=0;i<NMAT;i++) {
          Vec_DP d(num[i]),r(num[i]);
          Mat_DP v(num[i],num[i]);
          NR::jacobi(e[i],d,v,nrot);
          cout << "matrix number " << (i+1) << endl;
          cout << "number of JACOBI rotations: " << nrot << endl;
          cout << "eigenvalues: " << endl;
          for (j=0;j<num[i];j++) {
            cout << setw(12) << d[j];
            if ((j+1) % 5 == 0) cout << endl;
          }
          cout << endl << "eigenvectors:" << endl;
          for (j=0;j<num[i];j++) {
            cout << setw(9) << "number" << setw(4) << (j+1) << endl;
            for (k=0;k<num[i];k++) {
              cout << setw(12) << v[k][j];
              if ((k+1) % 5 == 0) cout << endl;
            }
            cout << endl;
          }
          // eigenvector test
          cout << endl << "eigenvector test" << endl;
          for (j=0;j<num[i];j++) {
            for (l=0;l<num[i];l++) {
              r[l]=0.0;
              for (k=0;k<num[i];k++) {
                if (k > l) {
                  kk=l;
                  ll=k;
                } else {
                  kk=k;
                  ll=l;
                }
                r[l] += (e[i][ll][kk]*v[k][j]);
              }
            }
            cout << "vector number " << (j+1) << endl;
            cout << setw(11) << "vector" << setw(14) << "mtrx*vec.";
            cout << setw(10) << "ratio" << endl;
            for (l=0;l<num[i];l++) {
              cout << setw(12) << v[l][j] << setw(12) << r[l];
              cout << setw(12) << r[l]/v[l][j] << endl;
            }
          }
          cout << "press RETURN to continue..." << endl;
          cin.get();
        }
        return 0;
}
