#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine frprmn

DP func(Vec_I_DP &x)
{
        return 1.0-NR::bessj0(x[0]-0.5)*NR::bessj0(x[1]-0.5)
                *NR::bessj0(x[2]-0.5);
}

void dfunc(Vec_I_DP &x, Vec_O_DP &df)
{
        df[0]=NR::bessj1(x[0]-0.5)*NR::bessj0(x[1]-0.5)*NR::bessj0(x[2]-0.5);
        df[1]=NR::bessj0(x[0]-0.5)*NR::bessj1(x[1]-0.5)*NR::bessj0(x[2]-0.5);
        df[2]=NR::bessj0(x[0]-0.5)*NR::bessj0(x[1]-0.5)*NR::bessj1(x[2]-0.5);
}

int main(void)
{
        const int NDIM=3;
        const DP FTOL=1.0e-6,PIO2=1.570796326794896619;
        int iter,k;
        DP angl,fret;
        Vec_DP p(NDIM);

        cout << "Program finds the minimum of a function" << endl;
        cout << "with different trial starting vectors." << endl;
        cout << "True minimum is (0.5,0.5,0.5)" << endl;
        cout << fixed << setprecision(4);
        for (k=0;k<5;k++) {
          angl=PIO2*k/4.0;
          p[0]=2.0*cos(angl);
          p[1]=2.0*sin(angl);
          p[2]=0.0;
          cout << endl << "Starting vector: (" << setw(6) << p[0] << ",";
          cout << setw(6) << p[1] << "," << setw(6) << p[2] << ")" << endl;
          NR::frprmn(p,FTOL,iter,fret,func,dfunc);
          cout << "Iterations: " << iter << endl;
          cout << "Solution vector: (" << setw(6) << p[0] << ",";
          cout << setw(6) << p[1] << "," << setw(6) << p[2] << ")" << endl;
          cout << "Func. value at solution: " << setw(14) << fret << endl;
        }
        return 0;
}
