#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine fpoly

int main(void)
{
        const int NVAL=5,NPOLY=5;
        const DP DX=0.1;
        int i,j;
        DP x;
        Vec_DP afunc(NPOLY);

        cout << endl << "powers of x" << endl;
        cout << setw(8) << "x" << setw(11) << "x**0";
        cout << setw(10) << "x**1" << setw(10) << "x**2";
        cout << setw(10) << "x**3" << setw(10) << "x**4" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<NVAL;i++) {
          x=(i+1)*DX;
          NR::fpoly(x,afunc);
          cout << setw(10) << x;
          for (j=0;j<NPOLY;j++) cout << setw(10) << afunc[j];
          cout << endl;
        }
        return 0;
}
