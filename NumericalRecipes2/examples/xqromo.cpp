#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine qromo

DP funcl(const DP x)
{
        return sqrt(x)/sin(x);
}

DP funcu(const DP x)
{
        static const DP PI=3.141592653589793238;

        return sqrt(PI-x)/sin(x);
}

DP fncinf(const DP x)
{
        return sin(x)/(x*x);
}

DP fncend(const DP x)
{
        return exp(-x)/sqrt(x);
}

int main(void)
{
        const DP X1=0.0, X3=3.141592653589793238;
        const DP X2=X3/2.0, AINF=1.0e20;
        DP res1,res2,result;

        cout << endl << "Improper integrals:" << endl << endl;
        result=NR::qromo(funcl,X1,X2,NR::midsql);
        cout << fixed << setprecision(6);
        cout << "Function: sqrt(x)/sin(x)       Interval: (0,pi/2)" << endl;
        cout << "Using: MIDSQL                  Result: "
          << setw(8) << result << endl << endl;
        result=NR::qromo(funcu,X2,X3,NR::midsqu);
        cout << "Function: sqrt(pi-x)/sin(x)    Interval: (pi/2,pi)" << endl;
        cout << "Using: MIDSQU                  Result: "
          << setw(8) << result << endl << endl;
        result=NR::qromo(fncinf,X2,AINF,NR::midinf);
        cout << "Function: sin(x)/x**2          Interval: (pi/2,infty)" << endl;
        cout << "Using: MIDINF                  Result: "
          << setw(8) << result << endl << endl;
        result=NR::qromo(fncinf,-AINF,-X2,NR::midinf);
        cout << "Function: sin(x)/x**2          Interval: (-infty,-pi/2)" << endl;
        cout << "Using: MIDINF                  Result: "
          << setw(8) << result << endl << endl;
        res1=NR::qromo(fncend,X1,X2,NR::midsql);
        res2=NR::qromo(fncend,X2,AINF,NR::midinf);
        cout << "Function: exp(-x)/sqrt(x)      Interval: (0.0,infty)" << endl;
        cout << "Using: MIDSQL,MIDINF           Result: "
          << setw(8) << (res1 + res2) << endl << endl;
        res2=NR::qromo(fncend,X2,AINF,NR::midexp);
        cout << "Function: exp(-x)/sqrt(x)      Interval: (0.0,infty)" << endl;
        cout << "Using: MIDSQL,MIDEXP           Result: "
          << setw(8) << (res1 + res2) << endl << endl;
        return 0;
}
