#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessk

int main(void)
{
        string txt;
        int i,nval,n;
        DP val,x;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Modified Bessel Function Kn")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Modified Bessel Function Kn" << endl;
        cout << setw(4) << "n" << setw(8) << "x";
        cout << setw(15) << "actual" << setw(20) << "bessk(n,x)";
        cout << endl << endl;
        for (i=0;i<nval;i++) {
          fp >> n >> x >> val;
          cout << setw(4) << n;
          cout << fixed << setprecision(2);
          cout << setw(9) << x;
          cout << scientific << setprecision(7);
          cout << setw(19) << val << setw(17) << NR::bessk(n,x) << endl;
        }
        fp.close();
        return 0;
}
