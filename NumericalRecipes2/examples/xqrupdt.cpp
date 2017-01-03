#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine qrupdt

int main(void)
{
        bool sing;
        int i,j,k,l,m,n;
        string txt;
        DP con;
        ifstream fp("matrx1.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx1.dat not found");
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> n >> m;
          getline(fp,txt);
          Vec_DP c(n),d(n),u(n),v(n);
          Mat_DP a(n,n),au(n,n),q(n,n),qt(n,n),r(n,n),s(n,n),x(n,n);
          getline(fp,txt);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) fp >> a[k][l];
          getline(fp,txt);
          getline(fp,txt);
          for (l=0;l<m;l++)
            for (k=0;k<n;k++) fp >> s[k][l];
          getline(fp,txt);
          getline(fp,txt);
          // Print out a-matrix for comparison with product of
          // Q and R decomposition matrices
          cout << "Original matrix:" << endl;
          cout << fixed << setprecision(6);
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << a[k][l];
            cout << endl;
          }
          // updated matrix we'll use later
          for (k=0;k<n;k++)
            for (l=0;l<n;l++)
              au[k][l]=a[k][l]+s[k][0]*s[l][1];
          // Perform the decomposition
          NR::qrdcmp(a,c,d,sing);
          if (sing) cerr << "Singularity in QR decomposition." << endl;
          // find the Q and R matrices
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) {
              if (l > k) {
                r[k][l]=a[k][l];
                q[k][l]=0.0;
              } else if (l < k) {
                r[k][l]=q[k][l]=0.0;
              } else {
                r[k][l]=d[k];
                q[k][l]=1.0;
              }
            }
          }
          for (i=n-2;i>=0;i--) {
            for (con=0.0,k=i;k<n;k++) con += a[k][i]*a[k][i];
            con /= 2.0;
            for (k=i;k<n;k++) {
              for (l=i;l<n;l++) {
                qt[k][l]=0.0;
                for (j=i;j<n;j++) {
                  qt[k][l] += q[j][l]*a[k][i]*a[j][i]/con;
                }
              }
            }
            for (k=i;k<n;k++)
              for (l=i;l<n;l++) q[k][l] -= qt[k][l];
          }
          // compute product of Q and R matrices for comparison
          // with original matrix.
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) {
              x[k][l]=0.0;
              for (j=0;j<n;j++)
                x[k][l] += q[k][j]*r[j][l];
            }
          }
          cout << endl << "Product of Q and R matrices:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << x[k][l];
            cout << endl;
          }
          cout << endl << "Q matrix of the decomposition:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << q[k][l];
            cout << endl;
          }
          cout << endl << "R matrix of the decomposition:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << r[k][l];
            cout << endl;
          }
          // Q transpose
          for (k=0;k<n;k++)
            for (l=0;l<n;l++)
              qt[k][l]=q[l][k];
          for (k=0;k<n;k++) {
            v[k]=s[k][1];
            for (u[k]=0.0,l=0;l<n;l++)
              u[k] += qt[k][l]*s[l][0];
          }
          NR::qrupdt(r,qt,u,v);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++)
              for (x[k][l]=0.0,j=0;j<n;j++) x[k][l] += qt[j][k]*r[j][l];
          cout << endl << "Updated matrix:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << au[k][l];
            cout << endl;
          }
          cout << endl << "Product of new Q and R matrices:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << x[k][l];
            cout << endl;
          }
          cout << endl << "New Q matrix:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << qt[l][k];
            cout << endl;
          }
          cout << endl << "New R matrix:" << endl;
          for (k=0;k<n;k++) {
            for (l=0;l<n;l++) cout << setw(12) << r[k][l];
            cout << endl;
          }
          cout << endl << "***********************************" << endl;
          cout << "press return for next problem:" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
