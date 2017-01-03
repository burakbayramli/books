#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine rj

int main(void)
{
        string txt;
        int i,nval;
        DP p,val,x,y,z;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Elliptic Integral Third Kind RJ")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Elliptic Integral Third Kind RJ" << endl;
        cout << setw(5) << "x" << setw(8) << "y" << setw(8) << "z";
        cout << setw(8) << "p" << setw(14) << "actual";
        cout << setw(17) << "rj(x,y,z,p)" << endl << endl;
        for (i=0;i<nval;i++) {
          fp >> x >> y >> z >> p >> val;
          cout << fixed << setprecision(2);
          cout << setw(8) << x << setw(8) << y;
          cout << setw(8) << z << setw(8) << p;
          cout << scientific << setprecision(6);
          cout << setw(14) << val;
          cout << setw(14) << NR::rj(x,y,z,p) << endl;
        }
        return 0;
}
