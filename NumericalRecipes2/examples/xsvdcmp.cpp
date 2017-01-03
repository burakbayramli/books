#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine svdcmp

int main(void)
{
        int j,k,l,m,n;
        string txt;
        ifstream fp("matrx3.dat");

        // read input matrices
        if (fp.fail())
          NR::nrerror("Data file matrx3.dat not found");
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> m >> n;
          getline(fp,txt);
          Vec_DP w(n);
          Mat_DP a(m,n),u(m,n),v(n,n);
          getline(fp,txt);
          // copy original matrix into u
          cout << fixed << setprecision(6);
          for (k=0;k<m;k++)
            for (l=0;l<n;l++) {
              fp >> a[k][l];
              u[k][l]=a[k][l];
            }
          getline(fp,txt);
          getline(fp,txt);
          // perform decomposition
          NR::svdcmp(u,w,v);
          // write results
          cout << "Decomposition matrices:" << endl;
          cout << "Matrix u" << endl;
          for (k=0;k<m;k++) {
            for (l=0;l<n;l++)
              cout << setw(12) << u[k][l];
            cout << endl;
          }
          cout << "Diagonal of matrix w" << endl;
          for (k=0;k<n;k++)
            cout << setw(12) << w[k];
          cout << endl << "Matrix v-transpose" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++)
              cout << setw(12) << v[l][k];
            cout << endl;
          }
          cout << endl << "Check product against original matrix:" << endl;
          cout << "Original matrix:" << endl;
          for (k=0;k<m;k++) {
            for (l=0;l<n;l++)
              cout << setw(12) << a[k][l];
            cout << endl;
          }
          cout << "Product u*w*(v-transpose):" << endl;
          for (k=0;k<m;k++) {
            for (l=0;l<n;l++) {
              a[k][l]=0.0;
              for (j=0;j<n;j++)
                a[k][l] += u[k][j]*w[j]*v[l][j];
            }
            for (l=0;l<n;l++) cout << setw(12) << a[k][l];
            cout << endl;
          }
          cout << "***********************************" << endl;
          cout << "press RETURN for next problem" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
