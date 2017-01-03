#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine ellpi

int main(void)
{
        const DP FAC=3.141592653589793238/180.0;
        string txt;
        int i,nval;
        DP ak,alpha,en,phi,val;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Legendre Elliptic Integral Third Kind")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Legendre Elliptic Integral Third Kind" << endl;
        cout << setw(6) << "phi" << setw(5) << "-en";
        cout << setw(12) << "sin(alpha)" << setw(11) << "actual";
        cout << setw(23) << "ellpi(phi,ak)" << endl << endl;
        for (i=0;i<nval;i++) {
          fp >> phi >> en >> alpha >> val;
          alpha=alpha*FAC;
          ak=sin(alpha);
          en = -en;
          phi=phi*FAC;
          cout << fixed << setprecision(2);
          cout << setw(6) << phi << setw(7) << en << setw(7) << ak;
          cout << scientific << setprecision(6);
          cout << setw(18) << val << setw(18) << NR::ellpi(phi,en,ak) << endl;
        }
        return 0;
}
