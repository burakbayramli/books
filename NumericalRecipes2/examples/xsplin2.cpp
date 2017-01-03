#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine splin2

int main(void)
{
        const int M=10,N=10;
        int i,j;
        DP f,ff,x1x2,xx1,xx2;
        Vec_DP x1(M),x2(N);
        Mat_DP y(M,N),y2(M,N);

        for (i=0;i<M;i++) x1[i]=0.2*(i+1);
        for (i=0;i<N;i++) x2[i]=0.2*(i+1);
        for (i=0;i<M;i++) {
          for (j=0;j<N;j++) {
            x1x2=x1[i]*x2[j];
            y[i][j]=x1x2*exp(-x1x2);
          }
        }
        NR::splie2(x1,x2,y,y2);
        cout << setw(9) << "x1" << setw(13) << "x2";
        cout << setw(15) << "splin2" << setw(13) << "actual" << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<10;i++) {
          xx1=0.1*(i+1);
          xx2=xx1*xx1;
          NR::splin2(x1,x2,y,y2,xx1,xx2,f);
          x1x2=xx1*xx2;
          ff=x1x2*exp(-x1x2);
          cout << setw(12) << xx1 << setw(13) << xx2;
          cout << setw(13) << f << setw(13) << ff << endl;
        }
        return 0;
}
