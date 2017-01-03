#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine gcf

int main(void)
{
        string txt;
        int i,nval;
        DP a,val,x,gammcf,gln;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Incomplete Gamma Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Incomplete Gamma Function" << endl;
        cout << setw(9) << "a" << setw(12) << "x" << setw(14) << "actual";
        cout << setw(13) << "gcf(a,x)" << setw(12) << "gammln(a)";
        cout << setw(9) << "gln" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> a >> x >> val;
          if (x >= (a+1.0)) {
            NR::gcf(gammcf,a,x,gln);
            cout << setw(12) << a << setw(12) << x << setw(12) << (1.0-val);
            cout << setw(12) << gammcf << setw(12) << NR::gammln(a);
            cout << setw(12) << gln << endl;
          }
        }
        return 0;
}
