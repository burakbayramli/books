#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine gaussj

int main(void)
{
        int j,k,l,m,n;
        string dummy;
        ifstream fp("matrx1.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx1.dat not found");
        cout << fixed << setprecision(6);
        getline(fp,dummy);
        while (!fp.eof()) {
          getline(fp,dummy);
          fp >> n >> m;
          fp.get();
          getline(fp,dummy);
          Mat_DP a(n,n),u(n,n),b(n,m),t(n,m);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) fp >> a[k][l];
          fp.get();
          getline(fp,dummy);
          for (l=0;l<m;l++)
            for (k=0;k<n;k++) fp >> b[k][l];
          fp.get();
          getline(fp,dummy);
          // save matrices for later testing of results
          Mat_DP ai=a;
          Mat_DP x=b;
          // invert matrix
          NR::gaussj(ai,x);
          cout << endl << "Inverse of matrix a : " << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << ai[k][l];
            cout << endl;
          }
          // check inverse
          cout << endl << "a times a-inverse:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) {
              u[k][l]=0.0;
              for (j=0;j<n;j++)
                u[k][l] += (a[k][j]*ai[j][l]);
            }
            for (l=0;l<n;l++) cout << setw(12) << u[k][l];
            cout << endl;
          }
          // check vector solutions
          cout << endl << "Check the following for equality:" << endl;
          cout << setw(21) << "original" << setw(15) << "matrix*sol'n" << endl;
          for (l=0;l<m;l++) {
            cout << "vector " << l << ": " << endl;
            for (k=0;k<n;k++) {
              t[k][l]=0.0;
              for (j=0;j<n;j++)
                t[k][l] += (a[k][j]*x[j][l]);
              cout << "        " << setw(13) << b[k][l];
              cout << setw(13) << t[k][l] << endl;
            }
          }
          cout << "***********************************" << endl;
          cout << "press RETURN for next problem:" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
