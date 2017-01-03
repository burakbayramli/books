#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine factln

int main(void)
{
        string txt;
        int i,n,nval;
        DP val;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("N-factorial")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "log of n_factorial" << endl;
        cout << setw(6) << "n" << setw(16) << "actual";
        cout << setw(16) << "factln(n)" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> n >> val;
          cout << setw(6) << n << setw(16) << log(val);
          cout << setw(16) << NR::factln(n) << endl;
        }
        return 0;
}
