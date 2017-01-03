#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessk0

int main(void)
{
        string txt;
        int i,nval;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Modified Bessel Function K0")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Modified Bessel Function K0" << endl;
        cout << setw(5) << "x" << setw(14) << "actual";
        cout << setw(19) << "bessk0(x)" << endl << endl;
        for (i=0;i < nval;i++) {
         fp >> x >> val;
         cout << fixed << setprecision(2);
         cout << setw(6) << x;
         cout << scientific << setprecision(7);
         cout << setw(17) << val << setw(17) << NR::bessk0(x) << endl;
        }
        fp.close();
        return 0;
}
