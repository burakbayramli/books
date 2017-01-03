#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessj

int main(void)
{
        string txt;
        int i,nval,n;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Bessel Function Jn")) {
          getline(fp,txt);
          if (fp.eof())
            NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Bessel Function Jn" << endl;
        cout << setw(3) << "n" << setw(3) << "x";
        cout << setw(12) << "actual";
        cout << setw(17) << "bessj(n,x)" << endl << endl;
        for (i=0;i < nval;i++) {
          fp >> n >> x >> val;
          cout << fixed << setprecision(0);
          cout << setw(3) << n << setw(3) << x;
          cout << scientific << setprecision(6);
          cout << setw(15) << val;
          cout << setw(15) << NR::bessj(n,x) << endl;
        }
        fp.close();
        return 0;
}
