#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine factrl

int main(void)
{
        string txt;
        int i,n,nval;
        DP actual;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("N-factorial")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        cout << endl << txt << endl;
        fp >> nval;
        getline(fp,txt);
        cout << setw(6) << "n" << setw(13) << "actual";
        cout << setw(17) << "factrl(n)" << endl << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> n >> actual;
          if (actual < 1.0e10) {
            cout << setw(6) << n << setw(16) << actual;
            cout << setw(16) << NR::factrl(n) << endl;
          } else {
            cout << setw(6) << n << setw(16) << actual;
            cout << setw(16) << NR::factrl(n) << endl;
          }
        }
        return 0;
}
