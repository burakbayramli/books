#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine zbrac

DP fx(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        bool succes;
        int i;
        DP x1,x2;

        cout << setw(21) << "bracketing values:";
        cout << setw(24) << "function values:" << endl << endl;
        cout << setw(9) << "x1" << setw(12) << "x2";
        cout << setw(21) << "bessj0(x1)" << setw(13) << "bessj0(x2)" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<10;i++) {
          x1=DP(i)+1.0;
          x2=x1+1.0;
          succes=NR::zbrac(fx,x1,x2);
          if (succes) {
            cout << setw(12) << x1 << setw(12) << x2;
            cout << setw(5) << " " << setw(12) << fx(x1);
            cout << setw(13) << fx(x2) << endl;
          }
        }
        return 0;
}
