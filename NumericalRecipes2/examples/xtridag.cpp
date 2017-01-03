#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine tridag

int main(void)
{
        int k,n;
        string txt;
        ifstream fp("matrx2.dat");

        if (fp.fail())
          NR::nrerror("Data file matrx2.dat not found");
        getline(fp,txt);
        while (!fp.eof()) {
          getline(fp,txt);
          fp >> n;
          getline(fp,txt);
          Vec_DP diag(n),superd(n),subd(n),rhs(n),u(n);
          getline(fp,txt);
          for (k=0;k<n;k++) fp >> diag[k];
          getline(fp,txt);
          getline(fp,txt);
          for (k=0;k<n-1;k++) fp >> superd[k];
          getline(fp,txt);
          getline(fp,txt);
          for (k=1;k<n;k++) fp >> subd[k];
          getline(fp,txt);
          getline(fp,txt);
          for (k=0;k<n;k++) fp >> rhs[k];
          getline(fp,txt);
          getline(fp,txt);
          // carry out solution
          NR::tridag(subd,diag,superd,rhs,u);
          cout << fixed << setprecision(6);
          cout << endl << "The solution vector is:" << endl;
          for (k=0;k<n;k++) cout << setw(12) << u[k];
          cout << endl;
          // test solution
          cout << endl << "(matrix)*(sol'n vector) should be:" << endl;
          for (k=0;k<n;k++) cout << setw(12) << rhs[k];
          cout << endl;
          cout << "actual result is:" << endl;
          for (k=0;k<n;k++) {
            if (k == 0)
              rhs[k]=diag[0]*u[0]+superd[0]*u[1];
            else if (k == n-1)
              rhs[k]=subd[n-1]*u[n-2]+diag[n-1]*u[n-1];
            else
              rhs[k]=subd[k]*u[k-1]+diag[k]*u[k]
                +superd[k]*u[k+1];
          }
          for (k=0;k<n;k++) cout << setw(12) << rhs[k];
          cout << endl;
          cout << "***********************************" << endl;
          cout << "press return for next problem:" << endl;
          cin.get();
        }
        fp.close();
        return 0;
}
