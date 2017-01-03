#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
#include "print_array.h"
using namespace std;

// Driver for routine selip

int main(void)
{
        const int NP=100;
        string txt;
        int i,k;
        DP q;
        Vec_DP a(NP),b(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        cout << endl << "original array:" << endl;
        cout << fixed << setprecision(2);
        print_array(a,10,7);
        for (i=0;i<NP;i++) b[i]=NR::selip(i,a);
        cout << endl << "sorted array:" << endl;
        print_array(b,10,7);
        for (;;) {
          cout << endl << "Input k (negative to end)" << endl;
          cin >> k;
          if (k < 0 || k >= NP) break;
          q=NR::selip(k,a);
          cout << "Element at sort index " << k;
          cout << " is " << setw(6) << q << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
