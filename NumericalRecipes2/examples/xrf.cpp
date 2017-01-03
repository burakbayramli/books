#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rf

int main(void)
{
        string txt;
        int i,nval;
        DP val,x,y,z;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Elliptic Integral First Kind RF")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Elliptic Integral First Kind RF" << endl;
        cout << setw(6) << "x" << setw(8) << "y" << setw(8) << "z";
        cout << setw(16) << "actual" << setw(17) << "rf(x,y,z)" << endl;
        for (i=0;i<nval;i++) {
          fp >> x >> y >> z >> val;
          cout << fixed << setprecision(2);
          cout << setw(8) << x << setw(8) << y;
          cout << setw(8) << z;
          cout << scientific << setprecision(6);
          cout << setw(16) << val << setw(16) << NR::rf(x,y,z) << endl;
        }
        return 0;
}
