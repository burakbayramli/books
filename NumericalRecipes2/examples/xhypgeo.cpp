#include <iostream>
#include <iomanip>
#include <cmath>
#include <complex>
#include "nr.h"
using namespace std;

// Driver for routine hypgeo

int main(void)
{
        DP x,y;
        complex<DP> a(0.5,0.0),b(1.0,0.0),c(1.5,0.0);
        complex<DP> z,zi,q1,q2,q3,q4;

        cout << fixed << setprecision(6);
        for (;;) {
          cout << "Input X Y of Complex Argument (or 0 0 to end):" << endl;
          cin >> x >> y;
          cout << endl;
          if ((x == 0.0) && (y == 0.0)) break;
          z=complex<DP> (x,y);
          q1=NR::hypgeo(a,b,c,z*z);
          q2=0.5*log((b+z)/(b-z))/z;
          q3=NR::hypgeo(a,b,c,-z*z);
          zi=complex<DP>(-y,x);
          q4=0.5*log((b+zi)/(b-zi))/zi;
          cout << "2F1(0.5,1.0,1.5;z^2) = ";
          cout << setw(11) << q1 << endl;
          cout << "check using log form:  ";
          cout << setw(12) << q2 << endl << endl;
          cout << "2F1(0.5,1.0,1.5;-z^2) = ";
          cout << setw(9) << q3 << endl;
          cout << "check using log form:   ";
          cout << setw(11) << q4 << endl << endl;
        }
        return 0;
}
