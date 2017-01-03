#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine betai

int main(void)
{
        string txt;
        int i,nval;
        DP a,b,x,val;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Incomplete Beta Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Incomplete Beta Function" << endl;
        cout << setw(9) << "a" << setw(12) << "b";
        cout << setw(12) << "x" << setw(14) << "actual";
        cout << setw(13) << "betai(x)" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> a >> b >> x >> val;
          cout << setw(12) << a << setw(12) << b << setw(12) << x;
          cout << setw(12) << val << setw(12) << NR::betai(a,b,x) << endl;
        }
        fp.close();
        return 0;
}
