#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine eulsum

int main(void)
{
        const int NVAL=40;
        int i,j,mval;
        DP sum,term,x,xpower;
        Vec_DP wksp(NVAL);

        // evaluate ln(1+x)=x-x^2/2+x^3/3-x^4/4 ... for -1<x<1
        cout << fixed << setprecision(6);
        for (;;) {
          cout << endl << "How many terms in polynomial?" << endl;
          cout << "Enter n between 1 and " << NVAL << ". (n=0 to end): ";
          cin >> mval;
          cout << endl;
          if ((mval < 1) || (mval > NVAL)) break;
          cout << setw(9) << "x" << setw(15) << "actual";
          cout << setw(15) << "polynomial" << endl;
          for (i = -8;i<=8;i++) {
            x=i/10.0;
            sum=0.0;
            xpower = -1;
            for (j=0;j<mval;j++) {
              xpower *= (-x);
              term=xpower/(j+1);
              NR::eulsum(sum,term,j,wksp);
            }
            cout << setw(12) << x << setw(13) << log(1.0+x);
            cout << setw(13) << sum << endl;
          }
        }
        return 0;
}
