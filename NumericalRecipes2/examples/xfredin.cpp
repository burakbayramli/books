#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fredin

DP g(const DP t)
{
        const DP PI=3.141592653589793238;

        return sqrt(t)-pow(PI/2.0,2.25)*pow(t,0.75)/2.25;
}

DP ak(const DP t, const DP s)
{
        return pow(t*s,0.75);
}

int main(void)
{
        const int N=8;
        const DP PI=3.141592653589793238;
        DP a=0.0,ans,b=PI/2.0,x;
        Vec_DP t(N),f(N),w(N);

        NR::fred2(a,b,t,f,w,g,ak);
        cout << fixed << setprecision(6);
        for (;;) {
          cout << "Enter T between 0 and PI/2 (or negative to end)" << endl;
          cin >> x;
          if (x < 0.0) break;
          ans=NR::fredin(x,a,b,t,f,w,g,ak);
          cout << "T, Calculated answer, True answer" << endl;
          cout << setw(10) << x << setw(11) << ans;
          cout << setw(11) << sqrt(x) << endl << endl;
        }
        return 0;
}
