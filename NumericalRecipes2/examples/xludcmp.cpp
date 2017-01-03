#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ludcmp

int main(void)
{
        int j,k,l,m,n,dum;
        string txt;
        DP d;
        ifstream fp("matrx1.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx1.dat not found");
        cout << fixed << setprecision(6);
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> n >> m;
          getline(fp,txt);
          Vec_INT indx(n),jndx(n);
          Mat_DP a(n,n),xl(n,n),xu(n,n),x(n,n);
          getline(fp,txt);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) fp >> a[k][l];
          getline(fp,txt);
          getline(fp,txt);
          for (l=0;l<m;l++)
            for (k=0;k<n;k++) fp >> x[k][l];
          getline(fp,txt);
          getline(fp,txt);
          // Print out a-matrix for comparison with product of
          // lower and upper decomposition matrices
          cout << "original matrix:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << a[k][l];
            cout << endl;
          }
          // Perform the decomposition
          NR::ludcmp(a,indx,d);
          // Compose separately the lower and upper matrices
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) {
              if (l > k) {
                xu[k][l]=a[k][l];
                xl[k][l]=0.0;
              } else if (l < k) {
                xu[k][l]=0.0;
                xl[k][l]=a[k][l];
              } else {
                xu[k][l]=a[k][l];
                xl[k][l]=1.0;
              }
            }
          }
          // Compute product of lower and upper matrices for
          // comparison with original matrix
          for (k=0;k<n;k++) {
            jndx[k]=k;
            for (l=0;l<n;l++) {
              x[k][l]=0.0;
              for (j=0;j<n;j++)
                x[k][l] += (xl[k][j]*xu[j][l]);
            }
          }
          cout << endl << "product of lower and upper ";
          cout << "matrices (rows unscrambled):" << endl;
          for (k=0;k<n;k++) {
            dum=jndx[indx[k]];
            jndx[indx[k]]=jndx[k];
            jndx[k]=dum;
          }
          for (k=0;k<n;k++)
            for (j=0;j<n;j++)
              if (jndx[j] == k) {
                for (l=0;l<n;l++)
                  cout << setw(12) << x[j][l];
                cout << endl;
              }
          cout << endl << "lower matrix of the decomposition:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++)
              cout << setw(12) << xl[k][l];
            cout << endl;
          }
          cout << endl << "upper matrix of the decomposition:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << xu[k][l];
            cout << endl;
          }
          cout << endl << "***********************************" << endl;
          cout << "press return for next problem:" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
