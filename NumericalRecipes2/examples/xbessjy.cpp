#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessjy

int main(void)
{
        string txt;
        int i,nval;
        DP rj,ry,rjp,ryp,x,xnu,xrj,xry,xrjp,xryp;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Ordinary Bessel Functions")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Ordinary Bessel Functions" << endl;
        cout << setw(5) << "xnu" << setw(8) << "x" << endl;
        cout << setw(14) << "rj" << setw(17) << "ry";
        cout << setw(18) << "rjp" << setw(17) << "ryp" << endl;
        cout << setw(14) << "xrj" << setw(17) << "xry";
        cout << setw(18) << "xrjp" << setw(17) << "xryp" << endl;
        for (i=0;i < nval;i++) {
          fp >> xnu >> x >> rj >> ry >> rjp >> ryp;
          NR::bessjy(x,xnu,xrj,xry,xrjp,xryp);
          cout << fixed << setprecision(2);
          cout << setw(5) << xnu << setw(6) << x << endl;
          cout << scientific << setprecision(6);
          cout << setw(16) << rj << setw(17) << ry;
          cout << setw(17) << rjp << setw(17) << ryp << endl;
          cout << setw(16) << xrj << setw(17) << xry;
          cout << setw(17) << xrjp << setw(17) << xryp << endl;
        }
        fp.close();
        return 0;
}
