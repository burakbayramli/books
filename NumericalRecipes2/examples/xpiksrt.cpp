#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
#include "print_array.h"
using namespace std;

// Driver for routine piksrt

int main(void)
{
        const int NP=100;
        string txt;
        int i;
        Vec_DP a(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        cout << "original array:" << endl;
        cout << fixed << setprecision(2);
        print_array(a,10,7);
        NR::piksrt(a);
        cout << "sorted array:" << endl;
        print_array(a,10,7);
        return 0;
}
