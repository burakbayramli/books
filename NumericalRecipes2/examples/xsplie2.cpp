#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine splie2

int main(void)
{
        const int M=10,N=10;
        int i,j;
        DP x1x2;
        Vec_DP x1(N),x2(N);
        Mat_DP y(M,N),y2(M,N);

        for (i=0;i<M;i++) x1[i]=0.2*(i+1);
        for (i=0;i<N;i++) x2[i]=0.2*(i+1);
        for (i=0;i<M;i++)
          for (j=0;j<N;j++) {
            x1x2=x1[i]*x2[j];
            y[i][j]=x1x2*x1x2;
          }
        NR::splie2(x1,x2,y,y2);
        cout << endl << "second derivatives from SPLIE2" << endl;
        cout << "natural spline assumed" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<5;i++) {
          for (j=0;j<5;j++) cout << setw(12) << y2[i][j];
          cout << endl;
        }
        cout << endl << "actual second derivatives" << endl;
        for (i=0;i<5;i++) {
          for (j=0;j<5;j++) cout << setw(12) << 2.0*x1[i]*x1[i];
          cout << endl;
        }
        return 0;
}
