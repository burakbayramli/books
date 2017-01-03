#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine fleg

int main(void)
{
        const int NVAL=5,NPOLY=5;
        const DP DX=0.2;
        int i,j;
        DP x;
        Vec_DP afunc(NPOLY);

        cout << endl << "Legendre polynomials" << endl;
        cout << setw(9) << "n=1" << setw(10) << "n=2";
        cout << setw(10) << "n=3" << setw(10) << "n=4";
        cout << setw(10) << "n=5" << endl;
        cout << fixed << setprecision(4);
        for (i=0;i<NVAL;i++) {
          x=(i+1)*DX;
          NR::fleg(x,afunc);
          cout << "x =" << setw(5) << x << endl;
          for (j=0;j<NPOLY;j++) cout << setw(10) << afunc[j];
          cout << "  routine FLEG" << endl;
          for (j=0;j<NPOLY;j++) cout << setw(10) << NR::plgndr(j,0,x);
          cout << "  routine PLGNDR" << endl << endl;
        }
        return 0;
}
