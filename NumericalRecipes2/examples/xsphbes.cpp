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
        int i,n,nval;
        DP sj,sy,sjp,syp,x,xsj,xsy,xsjp,xsyp;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Spherical Bessel Functions")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << txt << endl;
        cout << scientific << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> n >> x >> sj >> sy >> sjp >> syp;
          cout << setw(8) << "n" << setw(12) << "x" << endl;
          cout << setw(11) << "sj" << setw(16) << "sy" << setw(16) << "sjp";
          cout << setw(16) << "syp" << endl;
          cout << setw(11) << "xsj" << setw(16) << "xsy" << setw(16) << "xsjp";
          cout << setw(16) << "xsyp" << endl;
          NR::sphbes(n,x,xsj,xsy,xsjp,xsyp);
          cout << setw(8) << n << setw(16) << x << endl;
          cout << setw(16) << sj << setw(16) << sy << setw(16) << sjp;
          cout << setw(16) << syp << endl;
          cout << setw(16) << xsj << setw(16) << xsy << setw(16) << xsjp;
          cout << setw(16) << xsyp << endl << endl;
        }
        return 0;
}
