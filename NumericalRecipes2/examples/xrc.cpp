#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rc

int main(void)
{
        string txt;
        int i,nval;
        DP val,x,y;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Elliptic Integral Degenerate RC")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Elliptic Integral Degenerate RC" << endl;
        cout << setw(10) << "x" << setw(16) << "y";
        cout << setw(18) << "actual" << setw(17) << "rc(x,y)" << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> x >> y >> val;
          cout << setw(16) << x << setw(16) << y;
          cout << setw(16) << val << setw(16) << NR::rc(x,y) << endl;
        }
        return 0;
}
