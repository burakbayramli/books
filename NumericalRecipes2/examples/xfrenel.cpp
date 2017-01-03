#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine frenel

int main(void)
{
        string txt;
        int i,nval;
        DP x,xc,xs;
        complex<DP> cs;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Fresnel Integrals")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Fresnel Integrals" << endl;
        cout << setw(9) << "x" << setw(17) << "actual";
        cout << setw(15) << "c(x)" << setw(15) << "actual";
        cout << setw(15) << "s(x)" << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> x >> xs >> xc;
          NR::frenel(x,cs);
          cout << setw(15) << x << setw(15) << xs << setw(15) << imag(cs);;
          cout << setw(15) << xc << setw(15) << real(cs)<< endl;
        }
        return 0;
}
