#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ellf

int main(void)
{
        const DP FAC=3.141592653589793238/180.0;
        string txt;
        int i,nval;
        DP ak,alpha,phi,val;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Legendre Elliptic Integral First Kind")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Legendre Elliptic Integral First Kind" << endl;
        cout << setw(11) << "phi" << setw(20) << "sin(alpha)";
        cout << setw(14) << "actual" << setw(19) << "ellf(phi,ak)";
        cout << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> phi >> alpha >> val;
          alpha=alpha*FAC;
          ak=sin(alpha);
          phi=phi*FAC;
          cout << setw(16) << phi << setw(16) << ak;
          cout << setw(16) << val << setw(16) << NR::ellf(phi,ak) << endl;
        }
        return 0;
}
