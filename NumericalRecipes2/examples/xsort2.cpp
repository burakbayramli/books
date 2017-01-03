#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"
#include "print_array.h"
using namespace std;

// Driver for routine sort2

int main(void)
{
        const int NP=100;
        string txt;
        int i;
        Vec_DP a(NP),b(NP);
        ifstream fp("tarray.dat");

        if (fp.fail())
          NR::nrerror("Data file tarray.dat not found");
        getline(fp,txt);
        for (i=0;i<NP;i++) fp >> a[i];
        // generate b-array
        for (i=0;i<NP;i++) b[i]=i;
        // sort a and mix b
        NR::sort2(a,b);
        cout << endl << "After sorting a and mixing b, array a is:" << endl;
        cout << fixed << setprecision(2);
        print_array(a,10,7);
        cout << endl << "... and array b is:" << endl;
        print_array(b,10,7);
        cout << endl << "press return to continue..." << endl;
        cin.get();
        // sort b and mix a
        NR::sort2(b,a);
        cout << endl << "After sorting b and mixing a, array a is:" << endl;
        print_array(a,10,7);
        cout << endl << "... and array b is:" << endl;
        print_array(b,10,7);
        return 0;
}
