#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine mnbrak

DP func(const DP x)
{
        return NR::bessj0(x);
}

int main(void)
{
        int i;
        DP ax,bx,cx,fa,fb,fc;

        cout << fixed << setprecision(6);
        for (i=0;i<10;i++) {
          ax=i*0.5;
          bx=(i+1)*0.5;
          NR::mnbrak(ax,bx,cx,fa,fb,fc,func);
          cout << setw(14) << "a" << setw(13) << "b";
          cout << setw(13) << "c" << endl;
          cout << setw(3) << "x" << setw(15) << ax;
          cout << setw(13) << bx << setw(13) << cx << endl;
          cout << setw(3) << "f" << setw(15) << fa;
          cout << setw(13) << fb << setw(13) << fc << endl;
        }
        return 0;
}
