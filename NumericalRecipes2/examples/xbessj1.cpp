#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessj1

int main(void)
{
        string txt;
        int i,nval;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Bessel Function J1")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Bessel Function J1" << endl;
        cout << setw(3) << "x" << setw(12) << "actual";
        cout << setw(16) << "bessj1(x)" << endl << endl;
        for (i=0;i < nval;i++) {
          fp >> x >> val;
          cout << fixed << setprecision(0);
          cout << setw(3) << x;
          cout << scientific << setprecision(6);
          cout << setw(15) << val << setw(15) << NR::bessj1(x) << endl;
        }
        fp.close();
        return 0;
}
