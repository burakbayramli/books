#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine memcof

int main(void)
{
        const int N=1000,M=10;
        int i;
        DP pm;
        Vec_DP cof(M),data(N);
        ifstream fp("spctrl.dat");

        if (fp.fail())
          NR::nrerror("Data file spctrl.dat not found");
        for (i=0;i<N;i++) fp >> data[i];
        fp.close();
        NR::memcof(data,pm,cof);
        cout << "Coefficients for spectral estimation of spctrl.dat" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<M;i++) {
          cout << "a[" << setw(2) << i << "] = ";
          cout << setw(12) << cof[i] << endl;
        }
        cout << endl << "a0 = " << setw(12) << pm << endl;
        return 0;
}
