#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine beta

int main(void)
{
        string txt;
        int i,nval;
        DP val,w,z;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Beta Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Beta Function" << endl;
        cout << setw(9) << "w" << setw(12) << "z" << setw(14) << "actual";
        cout << setw(14) << "beta(w,z)" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> w >> z >> val;
          cout << setw(12) << w << setw(12) << z;
          cout << setw(12) << val << setw(12) << NR::beta(w,z) << endl;
        }
        fp.close();
        return 0;
}
