#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine plgndr

int main(void)
{
        string txt;
        int i,j,m,n,nval;
        DP fac,val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Legendre Polynomials")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Legendre Polynomials" << endl;
        cout << setw(4) << "n" << setw(4) << "m" << setw(10) << "x";
        cout << setw(18) << "actual" << setw(21) << "plgndr(n,m,x)" << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> n >> m >> x >> val;
          fac=1.0;
          if (m > 0)
            for (j=n-m+1;j<=n+m;j++) fac *= j;
          fac *= 2.0/(2.0*n+1.0);
          val *= sqrt(fac);
          cout << setw(4) << n << setw(4) << m << setw(16) << x;
          cout << setw(16) << val << setw(16) << NR::plgndr(n,m,x) << endl;
        }
        return 0;
}
