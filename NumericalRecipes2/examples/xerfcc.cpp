#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine erfcc

int main(void)
{
        string txt;
        int i,nval;
        DP x,val;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Error Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "complementary error function" << endl;
        cout << setw(9) << "x" << setw(14) << "actual";
        cout << setw(13) << "erfcc(x)" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> x >> val;
          val=1.0-val;
          cout << setw(12) << x << setw(12) << val;
          cout << setw(12) << NR::erfcc(x) << endl;
        }
        return 0;
}
