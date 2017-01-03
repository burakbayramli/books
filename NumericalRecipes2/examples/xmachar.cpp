#include <iostream>
#include <iomanip>
#include <limits>
#include "nr.h"
using namespace std;

typedef DP T;

// Driver for routine machar

int main(void)
{
        int ibeta,iexp,irnd,it,machep,maxexp,minexp,negep,ngrd;
        DP eps,epsneg,xmax,xmin;

        NR::machar(ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp,
          eps,epsneg,xmin,xmax);
        cout << setw(9) << " " << setw(15) << left << "machar";
        cout << setw(15) << "numeric_limits" << endl << endl;
        cout << "ibeta  = " << setw(15) << ibeta;
        cout << setw(15) << numeric_limits<T>::radix << endl;
        cout << "it     = " << setw(15) << it;
        cout << setw(15) << numeric_limits<T>::digits << endl;
        cout << "irnd   = " << setw(15) << irnd;
        switch (numeric_limits<T>::round_style) {
          case -1: cout << setw(20) << "indeterminate" << endl;
            break;
          case 0: cout << setw(20) << "round toward zero" << endl;
            break;
          case 1: cout << setw(20) << "round to nearest" << endl;
            break;
          case 2: cout << setw(20) << "round toward infty" << endl;
            break;
          case 3: cout << setw(20) << "round toward -infty" << endl;
        }
        cout << "ngrd   = " << setw(15) << ngrd << endl;
        cout << "machep = " << setw(15) << machep << endl;
        cout << "negep  = " << setw(15) << negep << endl;
        cout << "iexp   = " << setw(15) << iexp << endl;
        cout << "minexp = " << setw(15) << minexp;
        cout << setw(15) << numeric_limits<T>::min_exponent-1 << endl;
        cout << "maxexp = " << setw(15) << maxexp;
        cout << setw(15) << numeric_limits<T>::max_exponent << endl;
        cout << scientific << setprecision(6);
        cout << "eps    = " << setw(15) << eps;
        cout << setw(15) << numeric_limits<T>::epsilon() << endl;
        cout << "epsneg = " << setw(15) << epsneg << endl;
        cout << "xmin   = " << setw(15) << xmin;
        cout << setw(15) << numeric_limits<T>::min() << endl;
        cout << "xmax   = " << setw(15) << xmax;
        cout << setw(15) << numeric_limits<T>::max() << endl;
        return 0;
}
