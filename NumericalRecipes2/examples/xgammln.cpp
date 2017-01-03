#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine gammln

int main(void)
{
        string txt;
        int i,nval;
        DP actual,calc,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Gamma Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Gamma Function" << endl;
        cout << setw(10) << "x" << setw(22) << "actual";
        cout << setw(22) << "gammln(x)" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i < nval;i++) {
          fp >> x >> actual;
          if (x > 0.0) {
            calc=(x<1.0 ? NR::gammln(x+1.0)-log(x) : NR::gammln(x));
            cout << setw(12) << x << setw(21) << log(actual);
            cout << setw(21) << calc << endl;
          }
        }
        return 0;
}
