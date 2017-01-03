#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine ei

int main(void)
{
        string txt;
        int i,nval;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Exponential Integral Ei")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Exponential Integral Ei" << endl;
        cout << setw(9) << "x" << setw(16) << "actual";
        cout << setw(16) << "ei(x)" << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> x >> val;
          cout << setw(12) << x << setw(16) << val;
          cout << setw(16) << NR::ei(x) << endl;
        }
        return 0;
}
