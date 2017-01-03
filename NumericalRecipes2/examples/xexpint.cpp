#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine expint

int main(void)
{
        string txt;
        int i,nval,n;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Exponential Integral En")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Exponential Integral En" << endl;
        cout << setw(4) << "n" << setw(11) << "x" << setw(18) << "actual";
        cout << setw(19) << "expint(n,x)" << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> n >> x >> val;
          cout << setw(4) << n << setw(16) << x << setw(16) << val;
          cout << setw(16) << NR::expint(n,x) << endl;
        }
        return 0;
}
