#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine dfridr

DP func(const DP x)
{
        return tan(x);
}

int main(void)
{
        DP x,h,dx,err;

        cout << "input x, h (h=0.0 to stop):" << endl;
        cout << fixed << setprecision(6);
        cin >> x >> h;
        while (h > 0.0) {
          dx=NR::dfridr(func,x,h,err);
          cout << setw(11) << "dfridr" << setw(12) << "actual";
          cout << setw(12) << "error" << endl;
          cout << setw(12) << dx << setw(12) << 1.0/SQR(cos(x));
          cout << setw(12) << err << endl << endl;
          cout << "input x, h (h=0.0 to stop):" << endl;
          cin >> x >> h;
        }
        return 0;
}
