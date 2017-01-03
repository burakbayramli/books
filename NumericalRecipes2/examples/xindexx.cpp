#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
#include "print_array.h"
using namespace std;

// Driver for routine indexx

int main(void)
{
        const int NP=100;
        string txt;
        int i,j;
        Vec_INT indx(NP);
        Vec_DP a(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        NR::indexx(a,indx);
        cout << endl << "original array:" << endl;
        cout << fixed << setprecision(2);
        print_array(a,10,7);
        cout << endl << "sorted array" << endl;
        for (i=0;i<10;i++) {
          for (j=0;j<10;j++) cout << setw(7) << a[indx[10*i+j]];
          cout << endl;
        }
        return 0;
}
