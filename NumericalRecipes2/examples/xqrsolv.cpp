#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine qrsolv

int main(void)
{
        bool sing;
        int j,k,l,m,n;
        string txt;
        ifstream fp("matrx1.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx1.dat not found");
        cout << fixed << setprecision(6);
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> n >> m;
          getline(fp,txt);
          Vec_DP x(n),c(n),d(n);
          Mat_DP a(n,n),b(n,n),ai(n,n);
          getline(fp,txt);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) fp >> a[k][l];
          getline(fp,txt);
          getline(fp,txt);
          for (l=0;l<m;l++)
            for (k=0;k<n;k++) fp >> b[k][l];
          getline(fp,txt);
          getline(fp,txt);
          // Save matrix a for later testing
          for (l=0;l<n;l++)
            for (k=0;k<n;k++) ai[k][l]=a[k][l];
          // Do qr decomposition
          NR::qrdcmp(a,c,d,sing);
          if (sing) cerr << "Singularity in QR decomposition." << endl;
          // Solve equations for each right-hand vector
          for (k=0;k<m;k++) {
            for (l=0;l<n;l++) x[l]=b[l][k];
            NR::qrsolv(a,c,d,x);
            // Test results with original matrix
            cout << "right-hand side vector:" << endl;
            for (l=0;l<n;l++)
              cout << setw(12) << b[l][k];
            cout << endl << "result of matrix applied";
            cout << " to sol'n vector" << endl;
            for (l=0;l<n;l++) {
              b[l][k]=0.0;
              for (j=0;j<n;j++)
                b[l][k] += (ai[l][j]*x[j]);
            }
            for (l=0;l<n;l++)
              cout << setw(12) << b[l][k];
            cout << endl << "*********************************" << endl;
          }
          cout << "press RETURN for next problem:" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
