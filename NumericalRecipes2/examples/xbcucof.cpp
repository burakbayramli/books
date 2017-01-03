#include <iostream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine bcucof

int main(void)
{
        const DP x1_d[4]={0.0,2.0,2.0,0.0};
        const DP x2_d[4]={0.0,0.0,2.0,2.0};
        int i,j;
        DP d1,d2,ee,x1x2;
        Vec_DP y(4),y1(4),y2(4),y12(4);
        Vec_DP x1(x1_d,4),x2(x2_d,4);
        Mat_DP c(4,4);

        d1=x1[1]-x1[0];
        d2=x2[3]-x2[0];
        for (i=0;i<4;i++) {
          x1x2=x1[i]*x2[i];
          ee=exp(-x1x2);
          y[i]=x1x2*ee;
          y1[i]=x2[i]*(1.0-x1x2)*ee;
          y2[i]=x1[i]*(1.0-x1x2)*ee;
          y12[i]=(1.0-3.0*x1x2+x1x2*x1x2)*ee;
        }
        NR::bcucof(y,y1,y2,y12,d1,d2,c);
        cout << endl << "Coefficients for bicubic interpolation:";
        cout << endl << endl;
        cout << fixed << setprecision(6);
        for (i=0;i<4;i++) {
          for (j=0;j<4;j++) cout << setw(12) << c[i][j];
          cout << endl;
        }
        return 0;
}
