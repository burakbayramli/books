#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine polin2

int main(void)
{
        const int N=5;
        const DP PI=3.141592653589793238;
        int i,j;
        DP dy,f,x1,x2,y;
        Vec_DP x1a(N),x2a(N);
        Mat_DP ya(N,N);

        for (i=0;i<N;i++) {
          x1a[i]=(i+1)*PI/N;
          for (j=0;j<N;j++) {
            x2a[j]=1.0*(j+1)/N;
            ya[i][j]=sin(x1a[i])*exp(x2a[j]);
          }
        }
        // test 2-dimensional interpolation
        cout << endl << "Two dimensional interpolation of sin(x1)exp(x2)";
        cout << endl;
        cout << setw(9) << "x1" << setw(13) << "x2" << setw(14) << "f(x)";
        cout << setw(17) << "interpolated" << setw(12) << "error" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<4;i++) {
          x1=(-0.1+(i+1)/5.0)*PI;
          for (j=0;j<4;j++) {
            x2 = -0.1+(j+1)/5.0;
            f=sin(x1)*exp(x2);
            NR::polin2(x1a,x2a,ya,x1,x2,y,dy);
            cout << setw(12) << x1 << setw(13) << x2 << setw(13) << f;
            cout << setw(13) << y << setw(13) << dy << endl;
          }
          cout << endl << "***********************************" << endl;
        }
        return 0;
}
