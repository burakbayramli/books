#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine evlmem

int main(void)
{
        const int N=1000,M=10,NFDT=16;
        int i;
        DP fdt,pm;
        Vec_DP cof(M),data(N);
        ifstream fp("spctrl.dat");

        if (fp.fail())
          NR::nrerror("Data file spctrl.dat not found");
        for (i=0;i<N;i++) fp >> data[i];
        fp.close();
        NR::memcof(data,pm,cof);
        cout << "Power spectum estimate of data in spctrl.dat" << endl;
        cout << "     f*delta      power" << endl;
        cout << fixed << setprecision(6);
        for (fdt=0.0;fdt<=0.5;fdt+=0.5/NFDT) {
          cout << setw(12) << fdt;
          cout << setw(13) << NR::evlmem(fdt,cof,pm) << endl;
        }
        return 0;
}
