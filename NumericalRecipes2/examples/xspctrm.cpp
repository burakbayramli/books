#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine spctrm

int main(void)
{
        const int M=16;
        bool ovrlap;
        int j,k;
        Vec_DP p(M),q(M);
        ifstream fp("spctrl.dat");

        if (fp.fail())
          NR::nrerror("Data file spctrl.dat not found");
        k=8;
        ovrlap=true;
        NR::spctrm(fp,p,k,ovrlap);
        fp.seekg(0,ios_base::beg);
        k=16;
        ovrlap=false;
        NR::spctrm(fp,q,k,ovrlap);
        fp.close();
        cout << endl << "Spectrum of data in file spctrl.dat" << endl;
        cout << "index" << "        " << "overlapped " << "     " ;
        cout << "non-overlapped" << endl;
        cout << fixed << setprecision(6);
        for (j=0;j<M;j++) {
          cout << setw(3) << j << "      " << setw(13) << p[j];
          cout << "     " << setw(13) << q[j] << endl;
        }
        return 0;
}
