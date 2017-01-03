#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine svbksb, which calls routine svdcmp

int main(void)
{
        int j,k,l,m,n;
        string txt;
        DP wmax,wmin;
        ifstream fp("matrx1.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx1.dat not found");
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> n >> m;
          getline(fp,txt);
          Vec_DP w(n),x(n),c(n);
          Mat_DP a(n,n),b(n,m),u(n,n),v(n,n);
          getline(fp,txt);
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) fp >> a[k][l];
          getline(fp,txt);
          getline(fp,txt);
          for (l=0;l<m;l++)
            for (k=0;k<n;k++) fp >> b[k][l];
          getline(fp,txt);
          getline(fp,txt);
          // copy a into u
          for (k=0;k<n;k++)
            for (l=0;l<n;l++) u[k][l]=a[k][l];
          // decompose matrix a
          NR::svdcmp(u,w,v);
          // find maximum singular value
          wmax=0.0;
          for (k=0;k<n;k++)
            if (w[k] > wmax) wmax=w[k];
          // define "small"
          wmin=wmax*(1.0e-6);
          // zero the "small" singular values
          for (k=0;k<n;k++)
            if (w[k] < wmin) w[k]=0.0;
          // backsubstitute for each right-hand side vector
          cout << fixed << setprecision(6);
          for (l=0;l<m;l++) {
            cout << endl << "Vector number " << l << endl;
            for (k=0;k<n;k++) c[k]=b[k][l];
            NR::svbksb(u,w,v,c,x);
            cout << " solution vector is:" << endl;
            for (k=0;k<n;k++) cout << setw(12) << x[k];
            cout << endl << " original right-hand side vector:" << endl;
            for (k=0;k<n;k++) cout << setw(12) << c[k];
            cout << endl << " (matrix)*(sol'n vector):" << endl;
            for (k=0;k<n;k++) {
              c[k]=0.0;
              for (j=0;j<n;j++) c[k] += a[k][j]*x[j];
            }
            for (k=0;k<n;k++) cout << setw(12) << c[k];
            cout << endl;
          }
          cout << "***********************************" << endl;
          cout << "press RETURN for next problem" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
