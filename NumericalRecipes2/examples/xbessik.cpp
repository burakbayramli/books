#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bessik

int main(void)
{
        string txt;
        int i,nval;
        DP ri,rk,rip,rkp,x,xnu,xri,xrk,xrip,xrkp;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Modified Bessel Functions")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Modified Bessel Functions" << endl;
        cout << setw(5) << "xnu" << setw(8) << "x" << endl;
        cout << setw(14) << "ri" << setw(17) << "rk";
        cout << setw(18) << "rip" << setw(17) << "rkp" << endl;
        cout << setw(14) << "xri" << setw(17) << "xrk";
        cout << setw(18) << "xrip" << setw(17) << "xrkp" << endl;
        for (i=0;i < nval;i++) {
          fp >> xnu >> x >> ri >> rk >> rip >> rkp;
          NR::bessik(x,xnu,xri,xrk,xrip,xrkp);
          cout << fixed << setprecision(2);
          cout << setw(5) << xnu << setw(6) << x << endl;
          cout << scientific << setprecision(6);
          cout << setw(16) << ri << setw(17) << rk;
          cout << setw(17) << rip << setw(17) << rkp << endl;
          cout << setw(16) << xri << setw(17) << xrk;
          cout << setw(17) << xrip << setw(17) << xrkp << endl;
        }
        fp.close();
        return 0;
}
