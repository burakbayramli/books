#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine dawson

int main(void)
{
        string txt;
        int i,nval;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Dawson integral")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Dawson Integral" << endl << setw(9) << "x";
        cout << setw(14) << "actual" << setw(13) << "dawson(x)";
        cout << endl << endl << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> x >> val;
          cout << setw(12) << x << setw(12) << val;
          cout << setw(12) << NR::dawson(x) << endl;
        }
        return 0;
}
