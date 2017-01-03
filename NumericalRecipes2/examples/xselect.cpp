#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine select

int main(void)
{
        const int NP=100;
        string txt;
        int i,k;
        DP q,s;
        Vec_DP a(NP),b(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        cout << fixed << setprecision(2);
        for (;;) {
          cout << endl << "Input k (negative to end)" << endl;
          cin >> k;
          if (k < 0 || k >= NP) break;
          for (i=0;i<NP;i++) b[i]=a[i];
          s=NR::selip(k,a);
          q=NR::select(k,b);
          cout << "Element at sort index " << k;
          cout << " is " << setw(6) << q << endl;
          cout << "Cross-check from SELIP routine " << setw(6) << s << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
