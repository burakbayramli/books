#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine dftint

DP c,d;

inline DP coscxd(const DP x)
{
        return cos(c*x+d);
}

inline DP ci(const DP w, const DP x)
{
        return sin((w-c)*x-d)/(2.0*(w-c))+sin((w+c)*x+d)/(2.0*(w+c));
}

inline DP si(const DP w, const DP x)
{
        return (-cos((w-c)*x-d)/(2.0*(w-c))-cos((w+c)*x+d)/(2.0*(w+c)));
}

void getans(const DP a, const DP b, const DP w,
        DP &cans, DP &sans)
{
        cans=ci(w,b)-ci(w,a);
        sans=si(w,b)-si(w,a);
}

int main(void)
{
        DP w,a,b,cans,cosint,sans,sinint;

        cout << "input c,d:" << endl;
        cin >> c >> d;
        cout << "input a,b (or a=b to end):" << endl << endl;
        cin >> a >> b;
        cout << fixed << setprecision(6);
        if (a == b) return 0;
        cout << "  Omega  Integral cosine*(test func)    Err";
        cout << "    Integral sine*(test func)   Err" << endl << endl;
        for (;;) {
          cout << "input w (or < 0 to end):" << endl;
          cin >> w;
          if (w < 0.0) return 0;
          NR::dftint(coscxd,a,b,w,cosint,sinint);
          getans(a,b,w,cans,sans);
          cout << "  " << w << setw(16) << cans << setw(20) << (cosint-cans);
          cout << setw(16) << sans << setw(17) << (sinint-sans) << endl;
        }
}
