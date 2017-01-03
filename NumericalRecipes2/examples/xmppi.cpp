#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for mp routines

void mpsqr2(const int np)
{
        const unsigned int IAOFF=48,MACC=2;
        int j,n;
        string s;

        n=np+MACC;
        Vec_UCHR x(n),y(n),t(n);
        Vec_UCHR q(n),r(n);
        t[0]=2;
        for (j=1;j<n;j++) t[j]=0;
        NR::mpsqrt(x,x,t);
        NR::mpmov(y,x);
        cout << endl << "sqrt(2)   = ";
        s = y[0]+IAOFF;
        s += '.';
        // caution: next step is N**2. omit it for large N
        NR::mp2dfr(y,s);
        s.erase(2.408*np,s.length());
        cout << setw(64) << left << s << endl;
        cout << endl << "2-sqrt(2) = ";
        // Calculate this the hard way to exercise the mpdiv function
        NR::mpdiv(q,r,t,x);
        s = r[0]+IAOFF;
        s += '.';
        // caution: next step is N**2. omit it for large N
        NR::mp2dfr(r,s);
        s.erase(2.408*np,s.length());
        cout << setw(64) << left << s << endl;
}

int main(void)
{
        int n;

        for (;;) {
          cout << endl << "Input n (or n=0 to end):" << endl;
          cin >> n;
          if (n <= 0) break;
          mpsqr2(n);
          NR::mppi(n);
        }
        cout << "Normal completion" << endl;
        return 0;
}
