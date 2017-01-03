#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sncndn

int main(void)
{
        string txt;
        int i,nval;
        DP em,emmc,uu,val,sn,cn,dn;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Jacobian Elliptic Function")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Jacobian Elliptic Function" << endl;
        cout << setw(5) << "mc" << setw(12) << "u" << setw(14) << "actual";
        cout << setw(10) << "sn" << setw(17) << "sn^2+cn^2";
        cout << setw(19) << "(mc)*(sn^2)+dn^2" << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<nval;i++) {
          fp >> em >> uu >> val;
          emmc=1.0-em;
          NR::sncndn(uu,emmc,sn,cn,dn);
          cout << setw(5) << emmc << setw(12) << uu << setw(12) << val;
          cout << setw(12) << sn << setw(13) << (sn*sn+cn*cn);
          cout << setw(16) << (em*sn*sn+dn*dn) << endl;
        }
        return 0;
}
