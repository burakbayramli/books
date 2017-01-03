#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine hpsel

int main(void)
{
        const int NP=100;
        string txt;
        int i,k;
        DP check;
        Vec_DP a(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        cout << fixed << setprecision(2);
        for (;;) {
          cout << endl << "Input heap size (or 0 to end)" << endl;
          cin >> k;
          if (k < 1 || k >= NP) break;
          Vec_DP heap(k);
          NR::hpsel(a,heap);
          check=NR::select(NP-k,a);
          cout << "heap[0], check= " << setw(6) << heap[0];
          cout << setw(6) << check << endl;
          cout << "heap of numbers of size " << k << endl;
          for (i=0;i<k;i++)
            cout << setw(2) << i << setw(8) << heap[i] << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
