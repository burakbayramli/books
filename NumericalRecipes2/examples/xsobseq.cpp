#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine sobseq

int main(void)
{
        const int n2=3;
        int i,n1=(-1);
        Vec_DP x(n2);

        NR::sobseq(n1,x);
        cout << fixed << setprecision(5);
        for (i=0;i<32;i++) {
          NR::sobseq(n2,x);
          cout << setw(11) << x[0] << setw(11) << x[1];
          cout << setw(11) << x[2] << setw(6) << (i+1) << endl;
        }
        return 0;
}
