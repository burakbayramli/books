#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine bico

int main(void)
{
        string txt;
        int i,k,n,nval;
        DP binco;
        ifstream fp("fncval.dat");

        if (fp.fail())
          NR::nrerror("Data file fncval.dat not found");
        getline(fp,txt);
        while (txt.find("Binomial Coefficients")) {
          getline(fp,txt);
          if (fp.eof()) NR::nrerror("Data not found in fncval.dat");
        }
        fp >> nval;
        getline(fp,txt);
        cout << endl << "Binomial Coefficients" << endl;
        cout << setw(6) << "n" << setw(7) << "k" << setw(15) << "actual";
        cout << setw(15) << "bico(n,k)" << endl << endl;
        for (i=0;i<nval;i++) {
          fp >> n >> k >> binco;
          cout << setw(6) << n << setw(7) << k << setw(15) << binco;
          cout << setw(15) << NR::bico(n,k) << endl;
        }
        fp.close();
        return 0;
}
